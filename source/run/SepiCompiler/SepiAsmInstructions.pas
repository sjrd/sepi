unit SepiAsmInstructions;

interface

uses
  Windows, Classes, SysUtils, TypInfo, SepiReflectionCore, SepiMembers,
  SepiOpCodes, SepiAssembler, SepiReflectionConsts;

resourcestring
  SMultipleParamsWithSameSepiStackOffset =
    'Plusieurs paramètres ont la même valeur de SepiStackOffset';
  SParamsSepiStackOffsetsDontFollow =
    'Les SepiStackOffset des paramètres ne se suivent pas';
  SInvalidDataSize = 'Taille de données invalide';

type
  {*
    Paramètres invalide dans une instruction CALL
  *}
  ESepiInvalidParamsError = class(ESepiAssemblerError);

  {*
    Taille de données invalide pour un MOVE
  *}
  ESepiInvalidDataSizeError = class(ESepiAssemblerError);

  {*
    Instruction NOP
    @author sjrd
    @version 1.0
  *}
  TSepiAsmNope = class(TSepiAsmInstr)
  end;

  {*
    Instruction JUMP
    @author sjrd
    @version 1.0
  *}
  TSepiAsmJump = class(TSepiAsmInstr)
  private
    FDestination: TSepiJumpDest; /// Destination du JUMP
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiJumpDest read FDestination;
  end;

  {*
    Instruction JIT ou JIF (JUMP conditionnel)
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCondJump = class(TSepiAsmInstr)
  private
    FIfTrue: Boolean;            /// True donne un JIT, False donne un JIF
    FDestination: TSepiJumpDest; /// Destination du JUMP
    FTest: TSepiMemoryReference; /// Condition du saut
  public
    constructor Create(AOwner: TSepiAsmInstrList; AIfTrue: Boolean = True);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property IfTrue: Boolean read FIfTrue write FIfTrue;
    property Destination: TSepiJumpDest read FDestination;
    property Test: TSepiMemoryReference read FTest;
  end;

  {*
    Paramètre d'une instruction CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCallParam = class(TObject)
  private
    FName: string;                    /// Nom du paramètre
    FSepiStackOffset: Integer;        /// Offset dans la pile Sepi
    FParamSize: TSepiParamSize;       /// Taille du paramètre
    FStackUsage: Integer;             /// Nombre de DWord utilisés dans la pile
    FMemoryRef: TSepiMemoryReference; /// Référence mémoire

    FSize: Integer; /// Taille écrite dans le flux
  public
    constructor Create(AMethodAssembler: TSepiMethodAssembler;
      ASepiStackOffset: Integer; AParamSize: TSepiParamSize;
      const AName: string = '');
    destructor Destroy; override;

    procedure Make;
    procedure WriteToStream(Stream: TStream);

    property Name: string read FName;
    property SepiStackOffset: Integer read FSepiStackOffset;
    property ParamSize: TSepiParamSize read FParamSize;
    property StackUsage: Integer read FStackUsage;
    property MemoryRef: TSepiMemoryReference read FMemoryRef;

    property Size: Integer read FSize;
  end;

  {*
    Paramètres d'une instruction CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCallParams = class(TObject)
  private
    FMethodAssembler: TSepiMethodAssembler; /// Assembleur de méthode

    FParameters: array of TSepiAsmCallParam; /// Paramètres
    FResult: TSepiMemoryReference;           /// Résultat

    FSize: Integer; /// Taille écrite dans le flux

    procedure SortParameters;

    function GetCount: Integer;
    function GetParameters(Index: Integer): TSepiAsmCallParam;
    function GetParamByName(const Name: string): TSepiMemoryReference;
  public
    constructor Create(AMethodAssembler: TSepiMethodAssembler);
    destructor Destroy; override;

    procedure Prepare(Signature: TSepiSignature);

    function AddParam(SepiStackOffset: Integer;
      ParamSize: TSepiParamSize): TSepiMemoryReference;

    procedure Make;
    procedure WriteToStream(Stream: TStream);

    property MethodAssembler: TSepiMethodAssembler read FMethodAssembler;

    property Count: Integer read GetCount;
    property Parameters[Index: Integer]: TSepiAsmCallParam
      read GetParameters;
    property ParamByName[const Name: string]: TSepiMemoryReference
      read GetParamByName; default;
    property Result: TSepiMemoryReference read FResult;

    property Size: Integer read FSize;
  end;

  {*
    Instruction CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCall = class(TSepiAsmInstr)
  private
    FParameters: TSepiAsmCallParams; /// Paramètres
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Prepare(Signature: TSepiSignature); virtual;

    procedure Make; override;

    property Parameters: TSepiAsmCallParams read FParameters;
  end;

  {*
    Instruction Address CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmAddressCall = class(TSepiAsmCall)
  private
    FCallingConvention: TCallingConvention;   /// Convention d'appel
    FRegUsage: Byte;                          /// Utilisation des registres
    FResultBehavior: TSepiTypeResultBehavior; /// Comportement du résultat

    FAddress: TSepiMemoryReference; /// Adresse de la méthode à appeler
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Prepare(Signature: TSepiSignature); override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property CallingConvention: TCallingConvention
      read FCallingConvention write FCallingConvention;
    property RegUsage: Byte read FRegUsage write FRegUsage;
    property ResultBehavior: TSepiTypeResultBehavior
      read FResultBehavior write FResultBehavior;

    property Address: TSepiMemoryReference read FAddress;
  end;

  {*
    Instruction Static CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmStaticCall = class(TSepiAsmCall)
  private
    FMethodRef: Integer; /// Référence à la méthode
  public
    constructor Create(AOwner: TSepiAsmInstrList);

    procedure SetMethod(Method: TSepiMethod;
      PrepareParams: Boolean = True);

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property MethodRef: Integer read FMethodRef write FMethodRef;
  end;

  {*
    Instruction Dynamic CALL
    @author sjrd
    @version 1.0
  *}
  TSepiAsmDynamicCall = class(TSepiAsmCall)
  private
    FMethodRef: Integer;            /// Référence à la méthode
    FSelfMem: TSepiMemoryReference; /// Référence mémoire au Self
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure SetMethod(Method: TSepiMethod;
      PrepareParams: Boolean = True);

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property MethodRef: Integer read FMethodRef write FMethodRef;
    property SelfMem: TSepiMemoryReference read FSelfMem;
  end;

  {*
    Instruction LEA
    @author sjrd
    @version 1.0
  *}
  TSepiAsmLoadAddress = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FSource: TSepiMemoryReference;      /// Source
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property Source: TSepiMemoryReference read FSource;
  end;

  {*
    Instruction MOVE
    @author sjrd
    @version 1.0
  *}
  TSepiAsmMove = class(TSepiAsmInstr)
  private
    FDataSize: Word;      /// Taille des données à copier
    FDataType: TSepiType; /// Type des données à copier (si requis)

    FDestination: TSepiMemoryReference; /// Destination
    FSource: TSepiMemoryReference;      /// Source
  public
    constructor Create(AOwner: TSepiAsmInstrList;
      ADataSize: Word); overload;
    constructor Create(AOwner: TSepiAsmInstrList;
      ADataType: TSepiType); overload;
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property DataSize: Word read FDataSize;
    property DataType: TSepiType read FDataType;

    property Destination: TSepiMemoryReference read FDestination;
    property Source: TSepiMemoryReference read FSource;
  end;

  {*
    Instruction CVRT
    @author sjrd
    @version 1.0
  *}
  TSepiAsmConvert = class(TSepiAsmInstr)
  private
    FToType: TSepiBaseType;   /// Type de la destination
    FFromType: TSepiBaseType; /// Type de la source

    FDestination: TSepiMemoryReference; /// Destination
    FSource: TSepiMemoryReference;      /// Source
  public
    constructor Create(AOwner: TSepiAsmInstrList;
      AToType, AFromType: TSepiBaseType);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property ToType: TSepiBaseType read FToType;
    property FromType: TSepiBaseType read FFromType;

    property Destination: TSepiMemoryReference read FDestination;
    property Source: TSepiMemoryReference read FSource;
  end;

  {*
    Instruction opération
    @author sjrd
    @version 1.0
  *}
  TSepiAsmOperation = class(TSepiAsmInstr)
  private
    FVarType: TSepiBaseType; /// Type des variables
    FUseLeft: Boolean;       /// Utilise un opérande gauche
    FUseRight: Boolean;      /// Utilise un opérande droit

    FDestination: TSepiMemoryReference; /// Destination
    FLeft: TSepiMemoryReference;        /// Opérande gauche
    FRight: TSepiMemoryReference;       /// Opérande droit
  public
    constructor Create(AOwner: TSepiAsmInstrList; AOpCode: TSepiOpCode;
      AVarType: TSepiBaseType);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property VarType: TSepiBaseType read FVarType;
    property UseLeft: Boolean read FUseLeft;
    property UseRight: Boolean read FUseRight;

    property Destination: TSepiMemoryReference read FDestination;
    property Left: TSepiMemoryReference read FLeft;
    property Right: TSepiMemoryReference read FRight;

    /// Opérande unique
    property Source: TSepiMemoryReference read FLeft;
  end;

  {*
    Instruction de comparaison
    @author sjrd
    @version 1.0
  *}
  TSepiAsmCompare = class(TSepiAsmInstr)
  private
    FVarType: TSepiBaseType; /// Type des variables

    FDestination: TSepiMemoryReference; /// Destination
    FLeft: TSepiMemoryReference;        /// Opérande gauche
    FRight: TSepiMemoryReference;       /// Opérande droit
  public
    constructor Create(AOwner: TSepiAsmInstrList; AOpCode: TSepiOpCode;
      AVarType: TSepiBaseType);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property VarType: TSepiBaseType read FVarType;

    property Destination: TSepiMemoryReference read FDestination;
    property Left: TSepiMemoryReference read FLeft;
    property Right: TSepiMemoryReference read FRight;
  end;

  {*
    Instruction GTI, GDC ou GMC
    @author sjrd
    @version 1.0
  *}
  TSepiAsmGetRunInfo = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FReference: Integer;                /// Référence
  public
    constructor Create(AOwner: TSepiAsmInstrList; AOpCode: TSepiOpCode);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property Reference: Integer read FReference write FReference;
  end;

  {*
    Instruction IS
    @author sjrd
    @version 1.0
  *}
  TSepiAsmIsClass = class(TSepiAsmInstr)
  private
    FDestination: TSepiMemoryReference; /// Destination
    FMemObject: TSepiMemoryReference;   /// Objet mémoire
    FMemClass: TSepiMemoryReference;    /// Classe mémoire
    FClassRef: Integer;                 /// Référence à une classe (si msZero)
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property Destination: TSepiMemoryReference read FDestination;
    property MemObject: TSepiMemoryReference read FMemObject;
    property MemClass: TSepiMemoryReference read FMemClass;
    property ClassRef: Integer read FClassRef write FClassRef;
  end;

  {*
    Instruction AS
    @author sjrd
    @version 1.0
  *}
  TSepiAsmAsClass = class(TSepiAsmInstr)
  private
    FMemObject: TSepiMemoryReference; /// Objet mémoire
    FMemClass: TSepiMemoryReference;  /// Classe mémoire
    FClassRef: Integer;               /// Référence à une classe (si msZero)
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property MemObject: TSepiMemoryReference read FMemObject;
    property MemClass: TSepiMemoryReference read FMemClass;
    property ClassRef: Integer read FClassRef write FClassRef;
  end;

  {*
    Instruction RAISE
    @author sjrd
    @version 1.0
  *}
  TSepiAsmRaise = class(TSepiAsmInstr)
  private
    FExceptObject: TSepiMemoryReference; /// Objet exception
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property ExceptObject: TSepiMemoryReference read FExceptObject;
  end;

  {*
    Instruction RERS
    @author sjrd
    @version 1.0
  *}
  TSepiAsmReraise = class(TSepiAsmInstr)
  public
    constructor Create(AOwner: TSepiAsmInstrList);
  end;

  {*
    Instruction TRYE
    @author sjrd
    @version 1.0
  *}
  TSepiAsmTryExcept = class(TSepiAsmInstr)
  private
    FTryInstructions: TSepiAsmInstrList;    /// Instructions dans le try
    FExceptInstructions: TSepiAsmInstrList; /// Instructions dans le except
    FExceptObject: TSepiMemoryReference;    /// Objet exception

    FTrySize: Word;    /// Taille du try
    FExceptSize: Word; /// Taille du except
  protected
    function GetEndPosition: Integer; override;
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Make; override;
    procedure ComputeActualSize; override;
    procedure SetPosition(Value: Integer); override;
    procedure WriteToStream(Stream: TStream); override;

    property TryInstructions: TSepiAsmInstrList read FTryInstructions;
    property ExceptInstructions: TSepiAsmInstrList read FExceptInstructions;
    property ExceptObject: TSepiMemoryReference read FExceptObject;
  end;

  {*
    Instruction TRYF
    @author sjrd
    @version 1.0
  *}
  TSepiAsmTryFinally = class(TSepiAsmInstr)
  private
    FTryInstructions: TSepiAsmInstrList;     /// Instructions dans le try
    FFinallyInstructions: TSepiAsmInstrList; /// Instructions dans le finally

    FTrySize: Word;     /// Taille du try
    FFinallySize: Word; /// Taille du finally
  protected
    function GetEndPosition: Integer; override;
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    procedure Make; override;
    procedure ComputeActualSize; override;
    procedure SetPosition(Value: Integer); override;
    procedure WriteToStream(Stream: TStream); override;

    property TryInstructions: TSepiAsmInstrList read FTryInstructions;
    property FinallyInstructions: TSepiAsmInstrList read FFinallyInstructions;
  end;

  {*
    Clause ON d'un MultiOn
    @author sjrd
    @version 1.0
  *}
  TSepiAsmOnClause = record
    ClassRef: Integer;          /// Référence à une classe
    Destination: TSepiJumpDest; /// Destination du ON
  end;

  {*
    Instruction ON
    @author sjrd
    @version 1.0
  *}
  TSepiAsmMultiOn = class(TSepiAsmInstr)
  private
    FExceptObject: TSepiMemoryReference; /// Objet exception

    FOnClauses: array of TSepiAsmOnClause; /// Clauses ON

    function GetOnClauseCount: Integer;
    function GetOnClauses(Index: Integer): TSepiAsmOnClause;
  public
    constructor Create(AOwner: TSepiAsmInstrList);
    destructor Destroy; override;

    function AddOnClause(AClassRef: Integer): TSepiJumpDest; overload;
    function AddOnClause(SepiClass: TSepiClass): TSepiJumpDest; overload;

    procedure Make; override;
    procedure WriteToStream(Stream: TStream); override;

    property ExceptObject: TSepiMemoryReference read FExceptObject;

    property OnClauseCount: Integer read GetOnClauseCount;
    property OnClauses[Index: Integer]: TSepiAsmOnClause read GetOnClauses;
  end;

implementation

const
  /// Nombre maximum de paramètres pour avoir un taille en Byte (sinon Word)
  MaxParamCountForByteSize = 255 div 3; // 3 is max param size (Extended)

  /// OpCodes d'opérations unaires sur soi-même
  SelfUnaryOps = [ocSelfInc..ocSelfNeg];

  /// OpCodes d'opérations binaires sur soi-même
  SelfBinaryOps = [ocSelfAdd..ocSelfXor];

  /// OpCodes d'opérations unaires sur un autre
  OtherUnaryOps = [ocOtherInc..ocOtherNeg];

  /// OpCodes d'opérations binaires sur un autre
  OtherBinaryOps = [ocOtherAdd..ocOtherXor];

  /// OpCodes d'opérations
  OperationsOpCodes =
    SelfUnaryOps + SelfBinaryOps + OtherUnaryOps + OtherBinaryOps;

{--------------------}
{ TSepiAsmJump class }
{--------------------}

{*
  Crée une instruction JUMP
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmJump.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocJump;

  FDestination := TSepiJumpDest.Create(MethodAssembler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmJump.Destroy;
begin
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmJump.Make;
begin
  Destination.Make;
  FSize := SizeOf(TSepiOpCode) + SizeOf(Smallint);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmJump.WriteToStream(Stream: TStream);
begin
  inherited;
  Destination.WriteToStream(Stream, EndPosition);
end;

{------------------------}
{ TSepiAsmCondJump class }
{------------------------}

{*
  Crée une instruction JIT ou JIF
  @param AOwner    Liste d'instructions propriétaire
  @param AIfTrue   True donne un JIT plutôt qu'un JIF
*}
constructor TSepiAsmCondJump.Create(AOwner: TSepiAsmInstrList;
  AIfTrue: Boolean = True);
begin
  inherited Create(AOwner);

  FIfTrue := AIfTrue;
  FDestination := TSepiJumpDest.Create(MethodAssembler);
  FTest := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
    SizeOf(Boolean));
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCondJump.Destroy;
begin
  FTest.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCondJump.Make;
begin
  if IfTrue then
    FOpCode := ocJumpIfTrue
  else
    FOpCode := ocJumpIfFalse;

  Destination.Make;
  Test.Make;

  FSize := SizeOf(TSepiOpCode) + SizeOf(Smallint) + Test.Size;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCondJump.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream, EndPosition);
  Test.WriteToStream(Stream);
end;

{-------------------------}
{ TSepiAsmCallParam class }
{-------------------------}

{*
  Crée un paramètre
  @param AMethodAssembler   Assembleur de méthode Sepi
  @param ASepiStackOffset   Offset dans la pile Sepi
  @param AParamSize         Taille de paramètre
*}
constructor TSepiAsmCallParam.Create(AMethodAssembler: TSepiMethodAssembler;
  ASepiStackOffset: Integer; AParamSize: TSepiParamSize;
  const AName: string = '');
begin
  inherited Create;

  FName := AName;
  FSepiStackOffset := ASepiStackOffset;
  FParamSize := AParamSize;
  FMemoryRef := TSepiMemoryReference.Create(AMethodAssembler,
    aoAcceptAllConsts, ParamSize);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCallParam.Destroy;
begin
  FMemoryRef.Free;

  inherited;
end;

{*
  Construit le paramètre
*}
procedure TSepiAsmCallParam.Make;
const
  StackUsages: array[0..10] of Integer = (
    1 {address}, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3
  );
begin
  FSepiStackOffset := FSepiStackOffset and $FFFFFFFC;
  FStackUsage := StackUsages[FParamSize];

  MemoryRef.Make;
  FSize := SizeOf(TSepiParamSize) + MemoryRef.Size;
end;

{*
  Ecrit le paramètre dans un flux
  @param Stream   Flux de destination
*}
procedure TSepiAsmCallParam.WriteToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FParamSize, SizeOf(TSepiParamSize));
  MemoryRef.WriteToStream(Stream);
end;

{--------------------------}
{ TSepiAsmCallParams class }
{--------------------------}

{*
  Crée une liste de paramètres d'intruction CALL
*}
constructor TSepiAsmCallParams.Create(AMethodAssembler: TSepiMethodAssembler);
begin
  inherited Create;

  FMethodAssembler := AMethodAssembler;

  FResult := TSepiMemoryReference.Create(MethodAssembler, [aoZeroAsNil]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCallParams.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FParameters)-1 do
    FParameters[I].Free;

  inherited;
end;

{*
  Trie les paramètres selon l'ordre de leurs propriétés SepiStackOffset
*}
procedure TSepiAsmCallParams.SortParameters;
var
  I, J, ParamCount, Index: Integer;
  OrderedParams: array of TSepiAsmCallParam;
  Param, OldParam: TSepiAsmCallParam;
begin
  ParamCount := Length(FParameters);
  SetLength(OrderedParams, 3*ParamCount);
  FillChar(OrderedParams[0], 3*ParamCount*SizeOf(TSepiAsmCallParams), 0);

  // Fill OrderedParams and check for multiple params with same offset

  for I := 0 to ParamCount - 1 do
  begin
    Param := FParameters[I];
    Index := Param.SepiStackOffset div 4;

    for J := Index to Index + Param.StackUsage - 1 do
    begin
      if J >= Length(OrderedParams) then
        raise ESepiInvalidParamsError.CreateRes(
          @SParamsSepiStackOffsetsDontFollow);

      if OrderedParams[J] <> nil then
        raise ESepiInvalidParamsError.CreateRes(
          @SMultipleParamsWithSameSepiStackOffset);

      OrderedParams[J] := Param;
    end;
  end;

  // Rewrite FParameters

  OldParam := nil;
  J := 0;
  for I := 0 to 3 * ParamCount - 1 do
  begin
    Param := OrderedParams[I];
    if Param = nil then
      Break;
    if Param = OldParam then
      Continue;

    FParameters[J] := Param;
    Inc(J);
    OldParam := Param;
  end;

  // Ensure there is no forgotten parameters (it would be a hole in offsets)

  if J < ParamCount then
    raise ESepiInvalidParamsError.CreateRes(
      @SParamsSepiStackOffsetsDontFollow);
end;

{*
  Nombre de paramètres
  @return Nombre de paramètres
*}
function TSepiAsmCallParams.GetCount: Integer;
begin
  Result := Length(FParameters);
end;

{*
  Tableau zero-based des paramètres
  @param Index   Index d'un paramètre
  @return Paramètre à l'index spécifié
*}
function TSepiAsmCallParams.GetParameters(Index: Integer): TSepiAsmCallParam;
begin
  Result := FParameters[Index];
end;

{*
  Tableau des références mémoires des paramètres préparés indexés par leurs noms
  @param Name   Nom du paramètre
  @return Référence mémoire du paramètre
  @throws ESepiMetaNotFoundError Le paramètre n'a pas été trouvé
*}
function TSepiAsmCallParams.GetParamByName(
  const Name: string): TSepiMemoryReference;
var
  I: Integer;
begin
  for I := 0 to Length(FParameters)-1 do
  begin
    if AnsiSameText(FParameters[I].Name, Name) then
    begin
      Result := FParameters[I].MemoryRef;
      Exit;
    end;
  end;

  raise ESepiMetaNotFoundError.CreateResFmt(@SSepiObjectNotFound, [Name]);
end;

{*
  Prépare les paramètres en fonction d'une signature
  Préparer les paramètres a de multiples avantages. Il ne faut plus se
  préoccuper des offsets et des tailles des paramètres, ni de leur éventuel
  passage par adresse. De plus, pour les résultats qui sont passés comme
  paramètres, le fait de préparer les paramètres assigne à la propriété Result
  la référence mémoire à ce paramètre. Il n'y a donc plus de différences entre
  un résultat passé par adresse ou pas.
  @param Signature   Signature
*}
procedure TSepiAsmCallParams.Prepare(Signature: TSepiSignature);
var
  I: Integer;
  Param: TSepiParam;
  ParamSize: TSepiParamSize;
begin
  for I := 0 to Length(FParameters)-1 do
    FParameters[I].Free;
  SetLength(FParameters, Signature.ActualParamCount);

  for I := 0 to Signature.ActualParamCount-1 do
  begin
    Param := Signature.ActualParams[I];

    if Param.CallInfo.ByAddress then
      ParamSize := psByAddress
    else
      ParamSize := Param.ParamType.Size;

    FParameters[I] := TSepiAsmCallParam.Create(MethodAssembler,
      Param.CallInfo.SepiStackOffset, ParamSize, Param.Name);

    if Param.HiddenKind = hpResult then
      FResult := FParameters[I].MemoryRef;
  end;
end;

{*
  Ajoute un paramètre
  @param SepiStackOffset   Offset dans la pile Sepi
  @param ParamSize         Taille de paramètre
*}
function TSepiAsmCallParams.AddParam(SepiStackOffset: Integer;
  ParamSize: TSepiParamSize): TSepiMemoryReference;
var
  Index: Integer;
begin
  Index := Length(FParameters);
  SetLength(FParameters, Index+1);

  FParameters[Index] := TSepiAsmCallParam.Create(MethodAssembler,
    SepiStackOffset, ParamSize);
  Result := FParameters[Index].MemoryRef;
end;

{*
  Construit la liste des paramètres
*}
procedure TSepiAsmCallParams.Make;
var
  I, ParamCount: Integer;
begin
  // Make parameters
  FSize := 0;
  ParamCount := Length(FParameters);
  for I := 0 to ParamCount-1 do
  begin
    FParameters[I].Make;
    Inc(FSize, FParameters[I].Size);
  end;

  // Order parameters following their SepiStackOffset property
  SortParameters;

  // Make result
  Result.Make;
  Inc(FSize, Result.Size);

  // Head bytes
  if ParamCount > MaxParamCountForByteSize then
    Inc(FSize, SizeOf(Byte) + SizeOf(Word))
  else
    Inc(FSize, 2*SizeOf(Byte));
end;

{*
  Ecrit les paramètres dans un flux
  @param Stream   Flux de destination
*}
procedure TSepiAsmCallParams.WriteToStream(Stream: TStream);
var
  I, ParamCount, ParamsSize: Integer;
begin
  // Parameter count

  ParamCount := Length(FParameters);
  Stream.WriteBuffer(ParamCount, SizeOf(Byte));

  // Parameters size

  if ParamCount = 0 then
    ParamsSize := 0
  else
  begin
    with FParameters[ParamCount-1] do
      ParamsSize := SepiStackOffset div 4 + StackUsage;
  end;

  if ParamCount > MaxParamCountForByteSize then
    Stream.WriteBuffer(ParamsSize, SizeOf(Word))
  else
    Stream.WriteBuffer(ParamsSize, SizeOf(Byte));

  // Parameters

  for I := 0 to ParamCount-1 do
    FParameters[I].WriteToStream(Stream);

  // Result

  FResult.WriteToStream(Stream);
end;

{--------------------}
{ TSepiAsmCall class }
{--------------------}

{*
  Crée une instruction CALL
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmCall.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FParameters := TSepiAsmCallParams.Create(MethodAssembler)
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCall.Destroy;
begin
  FParameters.Free;

  inherited;
end;

{*
  Prépare les paramètres en fonction d'une signature
  @param Signature   Signature
*}
procedure TSepiAsmCall.Prepare(Signature: TSepiSignature);
begin
  Parameters.Prepare(Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCall.Make;
begin
  Parameters.Make;
  FSize := SizeOf(TSepiOpCode) + Parameters.Size;
end;

{---------------------------}
{ TSepiAsmAddressCall class }
{---------------------------}

{*
  Crée une instruction Basic CALL
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmAddressCall.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocAddressCall;

  FCallingConvention := ccRegister;
  FRegUsage := 0;
  FResultBehavior := rbNone;

  FAddress := TSepiMemoryReference.Create(MethodAssembler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmAddressCall.Destroy;
begin
  FAddress.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAddressCall.Prepare(Signature: TSepiSignature);
begin
  inherited;

  CallingConvention := Signature.CallingConvention;
  RegUsage := Signature.RegUsage;
  ResultBehavior := Signature.ReturnType.SafeResultBehavior;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAddressCall.Make;
begin
  inherited;

  Address.Make;

  Inc(FSize, SizeOf(TSepiCallSettings));
  Inc(FSize, Address.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAddressCall.WriteToStream(Stream: TStream);
var
  CallSettings: TSepiCallSettings;
begin
  inherited;

  CallSettings := CallSettingsEncode(CallingConvention, RegUsage,
    ResultBehavior);
  Stream.WriteBuffer(CallSettings, SizeOf(TSepiCallSettings));

  Address.WriteToStream(Stream);
  Parameters.WriteToStream(Stream);
end;

{--------------------------}
{ TSepiAsmStaticCall class }
{--------------------------}

{*
  Crée une instruction Static CALL
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmStaticCall.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocStaticCall;

  FMethodRef := 0;
end;

{*
  Renseigne la méthode à appeler
  @param Method     Méthode à appeler
  @param APrepare   Si True, prépare les paramètres
*}
procedure TSepiAsmStaticCall.SetMethod(Method: TSepiMethod;
  PrepareParams: Boolean = True);
begin
  FMethodRef := MethodAssembler.UnitAssembler.MakeReference(Method);

  if PrepareParams then
    Prepare(Method.Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmStaticCall.Make;
begin
  inherited;

  Inc(FSize, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmStaticCall.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FMethodRef, SizeOf(Integer));

  Parameters.WriteToStream(Stream);
end;

{---------------------------}
{ TSepiAsmDynamicCall class }
{---------------------------}

{*
  Crée une instruction Dynamic CALL
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmDynamicCall.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocDynamicCall;

  FMethodRef := 0;
  FSelfMem := TSepiMemoryReference.Create(MethodAssembler, [aoAcceptZero]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmDynamicCall.Destroy;
begin
  FSelfMem.Free;

  inherited;
end;

{*
  Renseigne la méthode à appeler
  @param Method     Méthode à appeler
  @param APrepare   Si True, prépare les paramètres
*}
procedure TSepiAsmDynamicCall.SetMethod(Method: TSepiMethod;
  PrepareParams: Boolean = True);
begin
  FMethodRef := MethodAssembler.UnitAssembler.MakeReference(Method);

  if PrepareParams then
    Prepare(Method.Signature);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmDynamicCall.Make;
begin
  inherited;

  SelfMem.Make;

  Inc(FSize, SizeOf(Integer));
  Inc(FSize, SelfMem.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmDynamicCall.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FMethodRef, SizeOf(Integer));

  SelfMem.WriteToStream(Stream);
  Parameters.WriteToStream(Stream);
end;

{---------------------------}
{ TSepiAsmLoadAddress class }
{---------------------------}

{*
  Crée une instruction LEA
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmLoadAddress.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocLoadAddress;

  FDestination := TSepiMemoryReference.Create(MethodAssembler);
  FSource := TSepiMemoryReference.Create(MethodAssembler,
    [aoAcceptAddressedConst]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmLoadAddress.Destroy;
begin
  FSource.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmLoadAddress.Make;
begin
  inherited;

  Destination.Make;
  Source.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Source.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmLoadAddress.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  Source.WriteToStream(Stream);
end;

{--------------------}
{ TSepiAsmMove class }
{--------------------}

{*
  Crée une instruction MOVE non typée
  @param AOwner      Liste d'instructions propriétaire
  @param ADataSize   Taille des données à copier
*}
constructor TSepiAsmMove.Create(AOwner: TSepiAsmInstrList; ADataSize: Word);
const
  SmallDataSizeOpCodes: array[1..10] of TSepiOpCode = (
    ocMoveByte, ocMoveWord, ocMoveSome, ocMoveDWord, ocMoveSome,
    ocMoveSome, ocMoveSome, ocMoveQWord, ocMoveSome, ocMoveExt
  );
begin
  if ADataSize = 0 then
    raise ESepiInvalidDataSizeError.CreateRes(@SInvalidDataSize);

  inherited Create(AOwner);

  FDataSize := ADataSize;
  FDataType := nil;

  case DataSize of
    1..10: FOpCode := SmallDataSizeOpCodes[DataSize];
    11..255: FOpCode := ocMoveSome;
  else
    FOpCode := ocMoveMany;
  end;

  FDestination := TSepiMemoryReference.Create(MethodAssembler);
  if DataSize <= SizeOf(Variant) then
  begin
    FSource := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
      DataSize);
  end else
  begin
    FSource := TSepiMemoryReference.Create(MethodAssembler,
      [aoAcceptAddressedConst]);
  end;
end;

{*
  Crée une instruction MOVE typée
  @param AOwner      Liste d'instructions propriétaire
  @param ADataType   Type des données à copier
*}
constructor TSepiAsmMove.Create(AOwner: TSepiAsmInstrList;
  ADataType: TSepiType);
begin
  if (not ADataType.NeedInit) and
    (CardinalSize(ADataType.Size) <= SizeOf(Word)) then
  begin
    Create(AOwner, ADataType.Size);
    Exit;
  end;

  inherited Create(AOwner);

  FDataSize := ADataType.Size;
  FDataType := ADataType;

  case DataType.Kind of
    tkLString: FOpCode := ocMoveAnsiStr;
    tkWString: FOpCode := ocMoveWideStr;
    tkInterface: FOpCode := ocMoveIntf;
    tkVariant: FOpCode := ocMoveVariant;
  else
    FOpCode := ocMoveOther;
  end;

  FDestination := TSepiMemoryReference.Create(MethodAssembler);

  if DataType.Kind in [tkLString, tkWString] then
  begin
    FSource := TSepiMemoryReference.Create(MethodAssembler,
      aoAcceptNonCodeConsts);
  end else if DataType.Kind in [tkInterface, tkVariant] then
    FSource := TSepiMemoryReference.Create(MethodAssembler, [aoAcceptZero])
  else
    FSource := TSepiMemoryReference.Create(MethodAssembler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmMove.Destroy;
begin
  FSource.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmMove.Make;
begin
  inherited;

  case OpCode of
    ocMoveSome: Inc(FSize, SizeOf(Byte));
    ocMoveMany: Inc(FSize, SizeOf(Word));
    ocMoveOther: Inc(FSize, SizeOf(Integer));
  end;

  Destination.Make;
  Source.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Source.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmMove.WriteToStream(Stream: TStream);
var
  DataTypeRef: Integer;
begin
  inherited;

  case OpCode of
    ocMoveSome: Stream.WriteBuffer(FDataSize, SizeOf(Byte));
    ocMoveMany: Stream.WriteBuffer(FDataSize, SizeOf(Word));
    ocMoveOther:
    begin
      DataTypeRef := UnitAssembler.MakeReference(DataType);
      Stream.WriteBuffer(DataTypeRef, SizeOf(Integer));
    end;
  end;

  Destination.WriteToStream(Stream);
  Source.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmConvert class }
{-----------------------}

{*
  Crée une instruction CVRT
  @param AOwner      Liste d'instructions propriétaire
  @param AToType     Type de destination
  @param AFromType   Type de la source
*}
constructor TSepiAsmConvert.Create(AOwner: TSepiAsmInstrList;
  AToType, AFromType: TSepiBaseType);
begin
  inherited Create(AOwner);

  FOpCode := ocConvert;

  FToType := AToType;
  FFromType := AFromType;

  FDestination := TSepiMemoryReference.Create(MethodAssembler);
  FSource := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
    BaseTypeConstSizes[FromType]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmConvert.Destroy;
begin
  FSource.Free;
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmConvert.Make;
begin
  inherited;

  Inc(FSize, 2*SizeOf(TSepiBaseType));

  Destination.Make;
  Source.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Source.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmConvert.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FToType, SizeOf(TSepiBaseType));
  Stream.WriteBuffer(FFromType, SizeOf(TSepiBaseType));

  Destination.WriteToStream(Stream);
  Source.WriteToStream(Stream);
end;

{-------------------------}
{ TSepiAsmOperation class }
{-------------------------}

{*
  Crée une instruction opération
  @param AOwner     Liste d'instructions propriétaire
  @param AOpCode    OpCode de l'instruction (doit être de type opération)
  @param AVarType   Type des variables
*}
constructor TSepiAsmOperation.Create(AOwner: TSepiAsmInstrList;
  AOpCode: TSepiOpCode; AVarType: TSepiBaseType);
begin
  if not (AOpCode in OperationsOpCodes) then
    RaiseInvalidOpCode;

  inherited Create(AOwner);

  FOpCode := AOpCode;

  FVarType := AVarType;
  FUseLeft := False;
  FUseRight := False;

  FDestination := TSepiMemoryReference.Create(MethodAssembler);

  if OpCode in SelfUnaryOps then
  begin
    FLeft := FDestination;
    FRight := FDestination;
  end else if OpCode in SelfBinaryOps then
  begin
    FUseRight := True;

    FLeft := FDestination;

    if OpCode in [ocSelfShl, ocSelfShr, ocSelfSar] then
    begin
      FRight := TSepiMemoryReference.Create(MethodAssembler,
        aoAcceptAllConsts, 1);
    end else
    begin
      FRight := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
        BaseTypeConstSizes[VarType]);
    end;
  end else if OpCode in OtherUnaryOps then
  begin
    FUseLeft := True;

    FLeft := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
      BaseTypeConstSizes[VarType]);
    FRight := FLeft;
  end else if OpCode in OtherBinaryOps then
  begin
    FUseLeft := True;
    FUseRight := True;

    FLeft := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
      BaseTypeConstSizes[VarType]);

    if OpCode in [ocOtherShl, ocOtherShr, ocOtherSar] then
    begin
      FRight := TSepiMemoryReference.Create(MethodAssembler,
        aoAcceptAllConsts, 1);
    end else
    begin
      FRight := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
        BaseTypeConstSizes[VarType]);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmOperation.Destroy;
begin
  FDestination.Free;
  if FUseLeft then
    FLeft.Free;
  if FUseRight then
    FRight.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmOperation.Make;
begin
  inherited;

  Inc(FSize, SizeOf(TSepiBaseType));

  Destination.Make;
  Inc(FSize, Destination.Size);

  if UseLeft then
  begin
    Left.Make;
    Inc(FSize, Left.Size);
  end;

  if UseRight then
  begin
    Right.Make;
    Inc(FSize, Right.Size);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmOperation.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FVarType, SizeOf(TSepiBaseType));

  Destination.WriteToStream(Stream);
  if UseLeft then
    Left.WriteToStream(Stream);
  if UseRight then
    Right.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmCompare class }
{-----------------------}

{*
  Crée une instruction de comparaison
  @param AOwner     Liste d'instructions propriétaire
  @param AOpCode    OpCode de l'instruction (doit être de type comparaison)
  @param AVarType   Type des variables
*}
constructor TSepiAsmCompare.Create(AOwner: TSepiAsmInstrList;
  AOpCode: TSepiOpCode; AVarType: TSepiBaseType);
begin
  if not (AOpCode in [ocCompEquals..ocCompGreaterEq]) then
    RaiseInvalidOpCode;

  inherited Create(AOwner);

  FOpCode := AOpCode;

  FVarType := AVarType;

  FDestination := TSepiMemoryReference.Create(MethodAssembler);
  FLeft := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
    BaseTypeConstSizes[VarType]);
  FRight := TSepiMemoryReference.Create(MethodAssembler, aoAcceptAllConsts,
    BaseTypeConstSizes[VarType]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmCompare.Destroy;
begin
  FDestination.Free;
  FLeft.Free;
  FRight.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCompare.Make;
begin
  inherited;

  Inc(FSize, SizeOf(TSepiBaseType));

  Destination.Make;
  Left.Make;
  Right.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, Left.Size);
  Inc(FSize, Right.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmCompare.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FVarType, SizeOf(TSepiBaseType));

  Destination.WriteToStream(Stream);
  Left.WriteToStream(Stream);
  Right.WriteToStream(Stream);
end;

{---------------------------}
{ TSepiAsmGetRunInfo class }
{---------------------------}

{*
  Crée une instruction GTI, GDC ou GMC
  @param AOwner     Liste d'instructions propriétaire
  @param AOpCode   OpCode de l'instruction (GTI, GDC ou GMC)
*}
constructor TSepiAsmGetRunInfo.Create(AOwner: TSepiAsmInstrList;
  AOpCode: TSepiOpCode);
begin
  if not (AOpCode in [ocGetTypeInfo, ocGetDelphiClass, ocGetMethodCode]) then
    RaiseInvalidOpCode;

  inherited Create(AOwner);

  FOpCode := AOpCode;

  FDestination := TSepiMemoryReference.Create(MethodAssembler);
  FReference := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmGetRunInfo.Destroy;
begin
  FDestination.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmGetRunInfo.Make;
begin
  inherited;

  Destination.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmGetRunInfo.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  Stream.WriteBuffer(FReference, SizeOf(Integer));
end;

{-----------------------}
{ TSepiAsmIsClass class }
{-----------------------}

{*
  Crée une instruction IS
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmIsClass.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocIsClass;

  FDestination := TSepiMemoryReference.Create(MethodAssembler);
  FMemObject := TSepiMemoryReference.Create(MethodAssembler, [aoAcceptZero]);
  FMemClass := TSepiMemoryReference.Create(MethodAssembler, [aoZeroAsNil]);
  FClassRef := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmIsClass.Destroy;
begin
  FDestination.Free;
  FMemObject.Free;
  FMemClass.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmIsClass.Make;
begin
  inherited;

  Destination.Make;
  MemObject.Make;
  MemClass.Make;

  Inc(FSize, Destination.Size);
  Inc(FSize, MemObject.Size);
  Inc(FSize, MemClass.Size);

  if MemClass.Space = msZero then
    Inc(FSize, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmIsClass.WriteToStream(Stream: TStream);
begin
  inherited;

  Destination.WriteToStream(Stream);
  MemObject.WriteToStream(Stream);
  MemClass.WriteToStream(Stream);

  if MemClass.Space = msZero then
    Stream.WriteBuffer(FClassRef, SizeOf(Integer));
end;

{-----------------------}
{ TSepiAsmAsClass class }
{-----------------------}

{*
  Crée une instruction AS
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmAsClass.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocAsClass;

  FMemObject := TSepiMemoryReference.Create(MethodAssembler, [aoAcceptZero]);
  FMemClass := TSepiMemoryReference.Create(MethodAssembler, [aoZeroAsNil]);
  FClassRef := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmAsClass.Destroy;
begin
  FMemObject.Free;
  FMemClass.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAsClass.Make;
begin
  inherited;

  MemObject.Make;
  MemClass.Make;

  Inc(FSize, MemObject.Size);
  Inc(FSize, MemClass.Size);

  if MemClass.Space = msZero then
    Inc(FSize, SizeOf(Integer));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmAsClass.WriteToStream(Stream: TStream);
begin
  inherited;

  MemObject.WriteToStream(Stream);
  MemClass.WriteToStream(Stream);

  if MemClass.Space = msZero then
    Stream.WriteBuffer(FClassRef, SizeOf(Integer));
end;

{---------------------}
{ TSepiAsmRaise class }
{---------------------}

{*
  Crée une instruction RAISE
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmRaise.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocRaise;

  FExceptObject := TSepiMemoryReference.Create(MethodAssembler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmRaise.Destroy;
begin
  FExceptObject.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmRaise.Make;
begin
  inherited;

  ExceptObject.Make;

  Inc(FSize, ExceptObject.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmRaise.WriteToStream(Stream: TStream);
begin
  inherited;

  ExceptObject.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmReraise class }
{-----------------------}

{*
  Crée une instruction RERS
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmReraise.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocReraise;
end;

{-------------------------}
{ TSepiAsmTryExcept class }
{-------------------------}

{*
  Crée une instruction TRYE
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmTryExcept.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocTryExcept;

  FTryInstructions := TSepiAsmInstrList.Create(MethodAssembler);
  FExceptInstructions := TSepiAsmInstrList.Create(MethodAssembler);
  FExceptObject := TSepiMemoryReference.Create(MethodAssembler, [aoZeroAsNil]);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmTryExcept.Destroy;
begin
  FExceptObject.Free;
  FExceptInstructions.Free;
  FTryInstructions.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiAsmTryExcept.GetEndPosition: Integer;
begin
  Result := Position + SizeOf(TSepiOpCode) + 2*SizeOf(Word) +
    ExceptObject.Size;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryExcept.Make;
begin
  inherited;

  Inc(FSize, 2*SizeOf(Word));

  ExceptObject.Make;
  Inc(FSize, ExceptObject.Size);

  TryInstructions.Make;
  Inc(FSize, TryInstructions.Size);

  ExceptInstructions.Make;
  Inc(FSize, ExceptInstructions.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryExcept.ComputeActualSize;
begin
  FSize := SizeOf(TSepiOpCode) + 2*SizeOf(Word) + ExceptObject.Size;

  TryInstructions.ComputeActualSize;
  FTrySize := TryInstructions.Size;
  Inc(FSize, FTrySize);

  ExceptInstructions.ComputeActualSize;
  FExceptSize := ExceptInstructions.Size;
  Inc(FSize, FExceptSize);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryExcept.SetPosition(Value: Integer);
begin
  inherited;

  Inc(Value, SizeOf(TSepiOpCode) + 2*SizeOf(Word) + ExceptObject.Size);

  TryInstructions.SetPosition(Value);
  Inc(Value, TryInstructions.Size);

  ExceptInstructions.SetPosition(Value);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryExcept.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FTrySize, SizeOf(Word));
  Stream.WriteBuffer(FExceptSize, SizeOf(Word));
  ExceptObject.WriteToStream(Stream);

  TryInstructions.WriteToStream(Stream);
  ExceptInstructions.WriteToStream(Stream);
end;

{--------------------------}
{ TSepiAsmTryFinally class }
{--------------------------}

{*
  Crée une instruction TRYF
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmTryFinally.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocTryFinally;

  FTryInstructions := TSepiAsmInstrList.Create(MethodAssembler);
  FFinallyInstructions := TSepiAsmInstrList.Create(MethodAssembler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmTryFinally.Destroy;
begin
  FFinallyInstructions.Free;
  FTryInstructions.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiAsmTryFinally.GetEndPosition: Integer;
begin
  Result := Position + SizeOf(TSepiOpCode) + 2*SizeOf(Word);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryFinally.Make;
begin
  inherited;

  Inc(FSize, 2*SizeOf(Word));

  TryInstructions.Make;
  Inc(FSize, TryInstructions.Size);

  FinallyInstructions.Make;
  Inc(FSize, FinallyInstructions.Size);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryFinally.ComputeActualSize;
begin
  FSize := SizeOf(TSepiOpCode) + 2*SizeOf(Word);

  TryInstructions.ComputeActualSize;
  FTrySize := TryInstructions.Size;
  Inc(FSize, FTrySize);

  FinallyInstructions.ComputeActualSize;
  FFinallySize := FinallyInstructions.Size;
  Inc(FSize, FFinallySize);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryFinally.SetPosition(Value: Integer);
begin
  inherited;

  Inc(Value, SizeOf(TSepiOpCode) + 2*SizeOf(Word));

  TryInstructions.SetPosition(Value);
  Inc(Value, TryInstructions.Size);

  FinallyInstructions.SetPosition(Value);
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmTryFinally.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FTrySize, SizeOf(Word));
  Stream.WriteBuffer(FFinallySize, SizeOf(Word));

  TryInstructions.WriteToStream(Stream);
  FinallyInstructions.WriteToStream(Stream);
end;

{-----------------------}
{ TSepiAsmMultiOn class }
{-----------------------}

{*
  Crée une instruction ON
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmMultiOn.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create(AOwner);

  FOpCode := ocMultiOn;

  FExceptObject := TSepiMemoryReference.Create(MethodAssembler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmMultiOn.Destroy;
begin
  FExceptObject.Free;

  inherited;
end;

{*
  Nombre de clauses ON
  @return Nombre de clauses ON
*}
function TSepiAsmMultiOn.GetOnClauseCount: Integer;
begin
  Result := Length(FOnClauses);
end;

{*
  Tableau zero-based des clauses ON
  @param Index   Index d'une clause
  @return Clause à l'index spécifié
*}
function TSepiAsmMultiOn.GetOnClauses(Index: Integer): TSepiAsmOnClause;
begin
  Result := FOnClauses[Index];
end;

{*
  Ajoute une clause ON
  @param AClassRef   Référence à la classe d'exception
  @return Destination du ON
*}
function TSepiAsmMultiOn.AddOnClause(AClassRef: Integer): TSepiJumpDest;
var
  Index: Integer;
begin
  Index := Length(FOnClauses);
  SetLength(FOnClauses, Index+1);

  with FOnClauses[Index] do
  begin
    ClassRef := AClassRef;
    Destination := TSepiJumpDest.Create(MethodAssembler);
    Result := Destination;
  end;
end;

{*
  Ajoute une clause ON
  @param SepiClass   Classe d'exception
  @return Destination du ON
*}
function TSepiAsmMultiOn.AddOnClause(SepiClass: TSepiClass): TSepiJumpDest;
begin
  Result := AddOnClause(
    MethodAssembler.UnitAssembler.MakeReference(SepiClass));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmMultiOn.Make;
begin
  inherited;

  ExceptObject.Make;

  Inc(FSize, ExceptObject.Size + SizeOf(Byte));
  Inc(FSize, Length(FOnClauses) * (SizeOf(Integer)+SizeOf(Smallint)));
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmMultiOn.WriteToStream(Stream: TStream);
var
  I, Count: Integer;
begin
  inherited;

  ExceptObject.WriteToStream(Stream);

  Count := Length(FOnClauses);
  Stream.WriteBuffer(Count, SizeOf(Byte));

  for I := 0 to Count-1 do
  begin
    with FOnClauses[I] do
    begin
      Stream.WriteBuffer(ClassRef, SizeOf(Integer));
      Destination.WriteToStream(Stream);
    end;
  end;
end;

end.

