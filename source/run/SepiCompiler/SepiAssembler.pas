{*
  Assembleur Sepi
  @author sjrd
  @version 1.0
*}
unit SepiAssembler;

interface

uses
  SysUtils, Classes, Contnrs, ScUtils, SepiReflectionCore, SepiMembers,
  SepiOpCodes, SepiReflectionConsts;

resourcestring
  SNoInstruction = 'Aucune instruction';
  SLabelNotFound = 'Label %d non trouvé';
  SLabelAlreadySet = 'Le label %d a déjà été assigné';
  SNamedLabelNotFound = 'Label ''%s'' non trouvé';
  SMixBetweenAssemblers = 'Mélange de données entre différents assembleurs';
  SMemoryCantBeZero = 'La référence mémoire ne peut être zéro';
  SMemoryCantBeConstant = 'La référence mémoire ne peut être constante';
  SMemoryCantBeTrueConst =
    'La référence mémoire ne peut être une vraie constante';
  SMemorySpaceOffsetMustBeWord =
    'L''offset d''espace mémoire doit être contenu dans un Word';
  SMemoryCantAccessObject = 'Impossible d''accéder à l''objet %s';
  STooManyOperations =
    'Une référence mémoire ne peut avoir que 15 opérations maximum';
  SZeroMemoryCantHaveOperations =
    'Une référence mémoire vers des 0 ne peut avoir d''opérations';
  SConstArgMustBeShort =
    'L''argument constant doit être contenu dans un Shortint';
  SJumpDestMustBeFixed = 'La destination du JUMP doit être fixe';
  SJumpMinFromPosGreaterThanMaxFromPos =
    'La position minimale (%d) est supérieure à la position maximale (%d)';
  SDestCantBeBetweenMinAndMaxFromPos =
    'La destination d''un JUMP ne peut être entre ses positions min et max';

type
  TSepiAsmInstrList = class;
  TSepiMethodAssembler = class;
  TSepiUnitAssembler = class;
  TSepiMemoryReference = class;

  {*
    Erreur d'assemblage Sepi
  *}
  ESepiAssemblerError = class(Exception);

  {*
    Label non trouvé lors de l'assemblage Sepi
  *}
  ESepiLabelError = class(ESepiAssemblerError);

  {*
    Référence mémoire invalide d'après l'instruction contenante
  *}
  ESepiInvalidMemoryReference = class(ESepiAssemblerError);

  {*
    Destination de JUMP invalide
  *}
  ESepiInvalidJumpDest = class(ESepiAssemblerError);

  {*
    Instruction d'assemblage Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiAsmInstr = class(TObject)
  private
    FOwner: TSepiAsmInstrList;              /// Liste propriétaire
    FMethodAssembler: TSepiMethodAssembler; /// Assembleur de code
    FUnitAssembler: TSepiUnitAssembler;     /// Assembleur d'unité

    FPosition: Integer; /// Position de l'instruction
  protected
    FOpCode: TSepiOpCode; /// OpCode
    FSize: Integer;       /// Taille

    function GetEndPosition: Integer; virtual;
  public
    constructor Create(AOwner: TSepiAsmInstrList);

    procedure AfterConstruction; override;

    procedure Make; virtual;
    procedure ComputeActualSize; virtual;
    procedure SetPosition(Value: Integer); virtual;
    procedure WriteToStream(Stream: TStream); virtual;

    property Owner: TSepiAsmInstrList read FOwner;
    property MethodAssembler: TSepiMethodAssembler read FMethodAssembler;
    property UnitAssembler: TSepiUnitAssembler read FUnitAssembler;

    property Position: Integer read FPosition;
    property OpCode: TSepiOpCode read FOpCode;
    property Size: Integer read FSize;
    property EndPosition: Integer read GetEndPosition;
  end;

  {*
    Liste d'instructions assembleur Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiAsmInstrList = class(TObject)
  private
    FMethodAssembler: TSepiMethodAssembler; /// Assembleur de code

    FInstructions: TObjectList; /// Instructions

    FSize: Integer; /// Taille

    function GetCount: Integer;
    function GetInstructions(Index: Integer): TSepiAsmInstr;
  public
    constructor Create(AMethodAssembler: TSepiMethodAssembler);
    destructor Destroy; override;

    procedure Make;
    procedure ComputeActualSize;
    procedure SetPosition(Value: Integer);
    procedure WriteToStream(Stream: TStream);

    property MethodAssembler: TSepiMethodAssembler read FMethodAssembler;

    property Count: Integer read GetCount;
    property Instructions[Index: Integer]: TSepiAsmInstr
      read GetInstructions; default;

    property Size: Integer read FSize;
  end;

  {*
    Informations d'initialisation d'une variable locale
    @author sjrd
    @version 1.0
  *}
  TLocalInfo = record
    TypeRef: Integer; /// Référence au type de la variable
    Offset: Integer;  /// Offset de la variable
  end;

  {*
    Assembleur du code d'une méthode Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiMethodAssembler = class(TObject)
  private
    FUnitAssembler: TSepiUnitAssembler; /// Assembleur d'unité

    FSepiMethod: TSepiMethod; /// Méthode Sepi correspondante
    FLocals: TSepiRecordType; /// Variables locales

    FInstructions: TSepiAsmInstrList; /// Instructions
    FLastInstruction: TSepiAsmInstr;  /// Dernière instruction ajoutée
    FSize: Integer;                   /// Taille totale (après assemblage)

    FLabels: TObjectList;   /// Labels numériques
    FNamedLabels: TStrings; /// Labels nommés (paire nom/instruction)

    FLocalsInfo: array of TLocalInfo; /// Informations sur les locales
  public
    constructor Create(AUnitAssembler: TSepiUnitAssembler;
      ASepiMethod: TSepiMethod);
    destructor Destroy; override;

    procedure AfterConstruction; override;

    function LookFor(const Name: string): TObject;

    function MakeLabel: Integer; overload;
    function MakeLabel(const LabelName: string): Integer; overload;

    procedure SetLabel(LabelID: Integer); overload;
    procedure SetLabel(const LabelName: string); overload;

    function FindLabel(LabelID: Integer): Integer; overload;
    function FindLabel(const LabelName: string): Integer; overload;

    procedure Assemble;
    procedure WriteToStream(Stream: TStream);
    procedure WriteLocalsInfo(Stream: TStream);

    property UnitAssembler: TSepiUnitAssembler read FUnitAssembler;
    property SepiMethod: TSepiMethod read FSepiMethod;
    property Locals: TSepiRecordType read FLocals;

    property Instructions: TSepiAsmInstrList read FInstructions;
    property Size: Integer read FSize;
  end;

  {*
    Assembleur d'unité Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiUnitAssembler = class(TObject)
  private
    FSepiUnit: TSepiUnit;  /// Unité Sepi
    FMethods: TObjectList; /// Assembleurs de méthodes

    FReferences: TObjectList; /// Références

    function GetMethodCount: Integer;
    function GetMethods(Index: Integer): TSepiMethodAssembler;
  public
    constructor Create(ASepiUnit: TSepiUnit);
    destructor Destroy; override;

    function MakeReference(Meta: TSepiMeta): Integer;

    procedure WriteToStream(Stream: TStream);

    property SepiUnit: TSepiUnit read FSepiUnit;

    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TSepiMethodAssembler read GetMethods;
  end;

  {*
    Record d'informations de déréférencement et d'opération sur adresse
    @author sjrd
    @version 1.0
  *}
  TSepiAddressDerefAndOpRec = record
    Dereference: TSepiAddressDereference;  /// Déréférencement
    Operation: TSepiAddressOperation;      /// Opération
    ConstOperationArg: Integer;            /// Argument constant de l'opération
    MemOperationArg: TSepiMemoryReference; /// Argument mémoire de l'opération
  end;

  {*
    Référence à un emplacement mémoire
    @author sjrd
    @version 1.0
  *}
  TSepiMemoryReference = class(TObject)
  private
    FMethodAssembler: TSepiMethodAssembler; /// Assembleur de méthode

    FOptions: TSepiAddressOptions; /// Options
    FConstSize: Integer;           /// Taille d'une constante

    FSpace: TSepiMemorySpace; /// Espace d'adressage
    FSpaceArgument: Integer;  /// Argument de l'espace d'adressage (offset)

    FOperations: array of TSepiAddressDerefAndOpRec; /// Opérations

    FConstant: Pointer; /// Constante (si Space = msConstant) - 0 par défaut

    FSize: Integer; /// Taille de la référence mémoire dans le code

    function GetOperationCount: Integer;
    function GetOperations(Index: Integer): TSepiAddressDerefAndOpRec;
  public
    constructor Create(AMethodAssembler: TSepiMethodAssembler;
      AOptions: TSepiAddressOptions = []; AConstSize: Integer = 0);
    destructor Destroy; override;

    procedure SetSpace(ASpace: TSepiMemorySpace;
      ASpaceArgument: Integer = 0); overload;
    procedure SetSpace(Meta: TSepiMeta); overload;
    procedure SetSpace(Param: TSepiParam; AutoDeref: Boolean = True); overload;
    procedure SetSpace(const Name: string;
      AutoDerefParam: Boolean = True); overload;

    procedure SetAsConst(Value: Int64); overload;
    procedure SetAsConst(Value: Boolean); overload;
    procedure SetAsConst(Value: Extended); overload;
    procedure SetAsConst(Value: Currency); overload;

    procedure ClearOperations;
    function AddOperation(ADereference: TSepiAddressDereference;
      AOperation: TSepiAddressOperation;
      AConstOperationArg: Integer = 0): TSepiMemoryReference; overload;
    procedure AddOperation(ADereference: TSepiAddressDereference); overload;
    function AddOperation(AOperation: TSepiAddressOperation;
      AConstOperationArg: Integer = 0): TSepiMemoryReference; overload;

    procedure GetConstant(var AConstant);
    procedure SetConstant(const AConstant);

    procedure Make;

    procedure WriteToStream(Stream: TStream);

    property MethodAssembler: TSepiMethodAssembler read FMethodAssembler;

    property Options: TSepiAddressOptions read FOptions;
    property ConstSize: Integer read FConstSize;
    property Space: TSepiMemorySpace read FSpace;
    property SpaceArgument: Integer read FSpaceArgument;

    property OperationCount: Integer read GetOperationCount;
    property Operations[Index: Integer]: TSepiAddressDerefAndOpRec
      read GetOperations;

    property Size: Integer read FSize;
  end;

  {*
    Destination d'un JUMP Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiJumpDest = class(TObject)
  private
    FMethodAssembler: TSepiMethodAssembler; /// Assembleur de méthode

    FLabelID: Integer;         /// ID du label
    FLabelName: string;        /// Nom du label

    procedure SetLabelID(Value: Integer);
    procedure SetLabelName(const Value: string);
  public
    constructor Create(AMethodAssembler: TSepiMethodAssembler);

    procedure Make;

    function MakeOffset(FromPos: Integer): Integer;

    procedure WriteToStream(Stream: TStream; FromPos: Integer); overload;
    procedure WriteToStream(Stream: TStream); overload;

    property MethodAssembler: TSepiMethodAssembler read FMethodAssembler;

    property LabelID: Integer read FLabelID write SetLabelID;
    property LabelName: string read FLabelName write SetLabelName;
  end;

const // don't localize
  LocalVarsName = '$Locals';  /// Nom du type record des variables locales
  ResultFieldName = 'Result'; /// Nom de la variable locale Result

const
  MaxOperationCount = $0F; /// Nombre maximum d'opérations sur une adresse

function IntegerSize(Value: Integer; ZeroGivesZero: Boolean = False): Integer;
function CardinalSize(Value: Cardinal;
  ZeroGivesZero: Boolean = False): Integer;

implementation

const
  /// Ensemble des opérations qui ont un argument mémoire
  OpsWithMemArg = [
    aoPlusMemShortint, aoPlusMemSmallint, aoPlusMemLongint,
    aoPlusConstTimesMemShortint, aoPlusConstTimesMemSmallint,
    aoPlusConstTimesMemLongint
  ];

  /// Le label n'a pas été assigné
  LabelUnassigned = TObject($FFFFFFFF);

{*
  Calcule la taille en octets d'un entier signé
  @param Value           Entier signé
  @param ZeroGivesZero   Si True, alors Value = 0 renvoie 0
  @return Taille de l'entier signé
*}
function IntegerSize(Value: Integer; ZeroGivesZero: Boolean = False): Integer;
begin
  if ZeroGivesZero and (Value = 0) then
    Result := 0
  else
  begin
    if Value < 0 then
      Value := not Value;

    if Value and Integer($FFFFFF80) = 0 then
      Result := 1
    else if Value and Integer($FFFF8000) = 0 then
      Result := 2
    else
      Result := 4;
  end;
end;

{*
  Calcule la taille en octets d'un entier non signé
  @param Value           Entier non signé
  @param ZeroGivesZero   Si True, alors Value = 0 renvoie 0
  @return Taille de l'entier non signé
*}
function CardinalSize(Value: Cardinal;
  ZeroGivesZero: Boolean = False): Integer;
begin
  if ZeroGivesZero and (Value = 0) then
    Result := 0
  else if Value and Cardinal($FFFFFF00) = 0 then
    Result := 1
  else if Value and Cardinal($FFFF0000) = 0 then
    Result := 2
  else
    Result := 4;
end;

{---------------------}
{ TSepiAsmInstr class }
{---------------------}

{*
  Crée une nouvelle instruction assembleur Sepi
  @param AOwner   Liste d'instructions propriétaire
*}
constructor TSepiAsmInstr.Create(AOwner: TSepiAsmInstrList);
begin
  inherited Create;

  FOwner := AOwner;
  FMethodAssembler := FOwner.MethodAssembler;
  FUnitAssembler := FMethodAssembler.UnitAssembler;

  FOpCode := ocNope;
end;

{*
  Position de la fin de l'instruction
  @return Position de la fin de l'instruction
*}
function TSepiAsmInstr.GetEndPosition: Integer;
begin
  Result := Position + Size;
end;

{*
  [@inheritDoc]
*}
procedure TSepiAsmInstr.AfterConstruction;
begin
  inherited;

  FOwner.FInstructions.Add(Self);
  FMethodAssembler.FLastInstruction := Self;
end;

{*
  Construit l'instruction
  A partir de l'appel à Make, l'instruction ne sera plus modifiée jusqu'à
  l'assemblage définitif.
  La méthode Make doit au minimum renseigner correctement la propriété Size.
  L'implémentation par défaut dans TSepiAsmInstr donne à MaxSize la taille d'un
  OpCode (1).
*}
procedure TSepiAsmInstr.Make;
begin
  FSize := SizeOf(TSepiOpCode);
end;

{*
  Calcule la taille réelle en tenant compte des compressions possibles
  Après l'appel à ComputeActualSize, la taille ne doit plus changer !
  L'implémentation par défaut dans TSepiAsmInstr ne fait rien.
*}
procedure TSepiAsmInstr.ComputeActualSize;
begin
end;

{*
  Donne sa position à l'instruction
  @param Value   Position
*}
procedure TSepiAsmInstr.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

{*
  Ecrit l'instruction dans un flux
  WriteToStream doit écrire exactement autant d'octets dans le flux que la
  valeur de la propriété Size.
  L'implémentation par défaut dans TSepiAsmInstr écrit l'OpCode
  @param Stream   Flux destination
*}
procedure TSepiAsmInstr.WriteToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FOpCode, SizeOf(TSepiOpCode));
end;

{-------------------------}
{ TSepiAsmInstrList class }
{-------------------------}

{*
  Crée une liste d'instructions
  @param AMethodAssembler   Assembleur de méthode
*}
constructor TSepiAsmInstrList.Create(AMethodAssembler: TSepiMethodAssembler);
begin
  inherited Create;

  FMethodAssembler := AMethodAssembler;
  FInstructions := TObjectList.Create;

  FSize := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiAsmInstrList.Destroy;
begin
  FInstructions.Free;

  inherited;
end;

{*
  Nombre d'instructions
  @return Nombre d'instructions dans la liste
*}
function TSepiAsmInstrList.GetCount: Integer;
begin
  Result := FInstructions.Count;
end;

{*
  Tableau zero-based des instructions
  @param Index   Index d'une instruction
  @return L'instruction à l'index spécifié
*}
function TSepiAsmInstrList.GetInstructions(Index: Integer): TSepiAsmInstr;
begin
  Result := TSepiAsmInstr(FInstructions[Index]);
end;

{*
  Construit les instructions
*}
procedure TSepiAsmInstrList.Make;
var
  I: Integer;
begin
  FSize := 0;
  for I := 0 to Count-1 do
  begin
    Instructions[I].Make;
    Inc(FSize, Instructions[I].Size);
  end;
end;

{*
  Calcule la taille réelle en tenant compte des compressions possibles
*}
procedure TSepiAsmInstrList.ComputeActualSize;
var
  I: Integer;
begin
  FSize := 0;
  for I := 0 to Count-1 do
  begin
    Instructions[I].ComputeActualSize;
    Inc(FSize, Instructions[I].Size);
  end;
end;

{*
  Donne leur position aux instructions
  @param Value   Position de base
*}
procedure TSepiAsmInstrList.SetPosition(Value: Integer);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
  begin
    Instructions[I].SetPosition(Value);
    Inc(Value, Instructions[I].Size);
  end;
end;

{*
  Ecrit l'instruction dans un flux
  WriteToStream doit écrire exactement autant d'octets dans le flux que la
  valeur de la propriété Size.
  @param Stream   Flux destination
*}
procedure TSepiAsmInstrList.WriteToStream(Stream: TStream);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Instructions[I].WriteToStream(Stream);
end;

{----------------------------}
{ TSepiMethodAssembler class }
{----------------------------}

{*
  Crée un nouvel assembleur de méthode Sepi
  Si la méthode possède déjà des variables locales - c'est-à-dire si un type
  record dont le nom est LocalVarsName est présent - celles-ci sont prises
  comme variables locales d'assemblage. Sinon, l'assembleur crée un record de
  variables locales, et le libèrera à sa destruction.
  @param ASepiMethod   Méthode Sepi
*}
constructor TSepiMethodAssembler.Create(AUnitAssembler: TSepiUnitAssembler;
  ASepiMethod: TSepiMethod);
begin
  inherited Create;

  FUnitAssembler := AUnitAssembler;
  FSepiMethod := ASepiMethod;

  // Fetch locals or create them if they don't exist

  FLocals := SepiMethod.GetMeta(LocalVarsName) as TSepiRecordType;
  if FLocals = nil then
    FLocals := TSepiRecordType.Create(SepiMethod, LocalVarsName);

  with SepiMethod.Signature do
  begin
    if (FLocals.ChildCount = 0) and
      (not (ReturnType.SafeResultBehavior in [rbNone, rbParameter])) then
      FLocals.AddField(ResultFieldName, ReturnType);
  end;

  // Initialize other fields

  FInstructions := TSepiAsmInstrList.Create(Self);
  FLastInstruction := nil;
  FSize := 0;

  FLabels := TObjectList.Create(False);
  FNamedLabels := TStringList.Create;
  TStringList(FNamedLabels).CaseSensitive := False;
end;

{*
  [@inheritDoc]
*}
destructor TSepiMethodAssembler.Destroy;
begin
  FNamedLabels.Free;
  FLabels.Free;
  FInstructions.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodAssembler.AfterConstruction;
begin
  inherited;

  UnitAssembler.FMethods.Add(Self);
end;

{*
  Cherche un objet à partir de son nom
  LookFor cherche parmi les variables locales, puis les paramètres, et enfin
  essaie la méthode LookFor de la méthode Sepi correspondante. Le résultat peut
  être de type TSepiMeta ou TSepiParam.
  @param Name   Nom de l'objet recherché
  @return Objet recherché, ou nil si non trouvé
*}
function TSepiMethodAssembler.LookFor(const Name: string): TObject;
begin
  Result := Locals.GetMeta(Name);
  if Result <> nil then
    Exit;

  Result := SepiMethod.Signature.GetParam(Name);
  if Result <> nil then
    Exit;

  Result := SepiMethod.LookFor(Name);
end;

{*
  Crée un nouveau label
  @return ID du label créé
*}
function TSepiMethodAssembler.MakeLabel: Integer;
begin
  Result := FLabels.Add(LabelUnassigned);
end;

{*
  Crée un nouveau label nommé
  @param LabelName   Nom du label
  @return ID du label créé
*}
function TSepiMethodAssembler.MakeLabel(const LabelName: string): Integer;
begin
  Result := FNamedLabels.IndexOf(LabelName);
  if Result < 0 then
  begin
    Result := MakeLabel;
    FNamedLabels.AddObject(LabelName, TObject(Result));
  end else
    Result := Integer(FNamedLabels.Objects[Result]);
end;

{*
  Assigne un label
  @param LabelID   ID du label
*}
procedure TSepiMethodAssembler.SetLabel(LabelID: Integer);
begin
  // Check label validity
  if (LabelID < 0) or (LabelID >= FLabels.Count) then
    raise ESepiLabelError.CreateResFmt(@SLabelNotFound, [LabelID]);
  if FLabels[LabelID] <> LabelUnassigned then
    raise ESepiLabelError.CreateResFmt(@SLabelAlreadySet, [LabelID]);

  // Set label
  FLabels[LabelID] := FLastInstruction;
end;

{*
  Assigne un label nommé
  @param LabelName   Nom du label
*}
procedure TSepiMethodAssembler.SetLabel(const LabelName: string);
begin
  SetLabel(MakeLabel(LabelName));
end;

{*
  Cherche un label
  @param LabelID   ID du label recherché
  @return Position du label
  @throws ESepiLabelError Le label n'a pas été trouvé
*}
function TSepiMethodAssembler.FindLabel(LabelID: Integer): Integer;
var
  Instruction: TSepiAsmInstr;
begin
  if (LabelID < 0) or (LabelID >= FLabels.Count) then
    raise ESepiLabelError.CreateResFmt(@SLabelNotFound, [LabelID]);
  if FLabels[LabelID] = LabelUnassigned then
    raise ESepiLabelError.CreateResFmt(@SLabelNotFound, [LabelID]);

  Instruction := TSepiAsmInstr(FLabels[LabelID]);
  if Instruction = nil then
    Result := 0
  else
    Result := Instruction.EndPosition;
end;

{*
  Cherche un label nommé
  @param LabelName   Nom du label recherché
  @return Position du label
  @throws ESepiLabelError Le label n'a pas été trouvé
*}
function TSepiMethodAssembler.FindLabel(const LabelName: string): Integer;
begin
  try
    Result := FindLabel(MakeLabel(LabelName));
  except
    on Error: ESepiLabelError do
      raise ESepiLabelError.CreateResFmt(@SNamedLabelNotFound, [LabelName]);
  end;
end;

{*
  Assemble les instructions
*}
procedure TSepiMethodAssembler.Assemble;
var
  Count: Integer;
  I: Integer;
  LocalVar: TSepiField;
begin
  // Local variables must have been completed
  if not Locals.Completed then
    Locals.Complete;

  // Assemble
  Instructions.Make;
  Instructions.SetPosition(0);
  Instructions.ComputeActualSize;
  Instructions.SetPosition(0);
  FSize := Instructions.Size;

  // Make locals info
  SetLength(FLocalsInfo, Locals.ChildCount);
  Count := 0;
  for I := 0 to Locals.ChildCount-1 do
  begin
    // Ignore non-field children
    if not (Locals.Children[I] is TSepiField) then
      Continue;

    // Ignore variables whose type doesn't require initialization
    LocalVar := TSepiField(Locals.Children[I]);
    if not LocalVar.FieldType.NeedInit then
      Continue;

    // Add an item to locals info
    with FLocalsInfo[Count] do
    begin
      TypeRef := UnitAssembler.MakeReference(LocalVar.FieldType);
      Offset := LocalVar.Offset;
      Inc(Count);
    end;
  end;
  SetLength(FLocalsInfo, Count);
end;

{*
  Ecrit la méthode dans un flux (tel que TSepiRuntimeMethod puisse le lire)
  La méthode doit avoir été assemblée au préalable via la méthode Assemble.
  @param Stream   Flux de destination
*}
procedure TSepiMethodAssembler.WriteToStream(Stream: TStream);
var
  NearlyFullName: string;
  Size: Integer;
begin
  // Write name
  NearlyFullName := SepiMethod.GetFullName;
  Delete(NearlyFullName, 1, Pos('.', NearlyFullName));
  WriteStrToStream(Stream, NearlyFullName);

  // Write parameters, locals and code sizes
  Size := SepiMethod.Signature.SepiStackUsage;
  Stream.WriteBuffer(Size, 4);
  Size := Locals.Size;
  if Size and 3 <> 0 then
    Size := (Size and (not 3)) + 4;
  Stream.WriteBuffer(Size, 4);
  Stream.WriteBuffer(FSize, 4);

  // Write code
  Instructions.WriteToStream(Stream);
end;

{*
  Ecrit les informations d'initialisation des variables locales
  @param Stream   Flux de destination
*}
procedure TSepiMethodAssembler.WriteLocalsInfo(Stream: TStream);
var
  Count: Integer;
begin
  Count := Length(FLocalsInfo);
  Stream.WriteBuffer(Count, 4);

  Stream.WriteBuffer(FLocalsInfo[0], Count*SizeOf(TLocalInfo));
end;

{--------------------------}
{ TSepiUnitAssembler class }
{--------------------------}

{*
  Crée un nouvel assembleur d'unité Sepi
  @param ASepiUnit   Unité Sepi à assembler
*}
constructor TSepiUnitAssembler.Create(ASepiUnit: TSepiUnit);
begin
  inherited Create;

  FSepiUnit := ASepiUnit;
  FMethods := TObjectList.Create;
  FReferences := TObjectList.Create(False);
end;

{*
  [@inheritDoc]
*}
destructor TSepiUnitAssembler.Destroy;
begin
  FReferences.Free;
  FMethods.Free;

  inherited;
end;

{*
  Nombre d'assembleurs de méthode
  @return Nombre d'assembleurs de méthode
*}
function TSepiUnitAssembler.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

{*
  Tableau zero-based des assembleurs de méthode
  @param Index   Index d'un assembleur
  @return Assembleur à l'index spécifié
*}
function TSepiUnitAssembler.GetMethods(Index: Integer): TSepiMethodAssembler;
begin
  Result := TSepiMethodAssembler(FMethods[Index]);
end;

{*
  Construit un numéro de référence à un meta Sepi
  @param Meta   Meta pour lequel construire un numéro de référence
  @return Numéro de référence du meta
*}
function TSepiUnitAssembler.MakeReference(Meta: TSepiMeta): Integer;
begin
  Result := FReferences.IndexOf(Meta);
  if Result < 0 then
    Result := FReferences.Add(Meta);
end;

{*
  Ecrit l'unité assemblée dans un flux
  @param Stream   Flux destination
*}
procedure TSepiUnitAssembler.WriteToStream(Stream: TStream);
var
  I, Count: Integer;
begin
  // Assemble methods
  for I := 0 to MethodCount-1 do
    Methods[I].Assemble;

  // Write methods
  Count := MethodCount;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
    Methods[I].WriteToStream(Stream);

  // Save Sepi unit
  SepiUnit.SaveToStream(Stream);

  // Write references
  Count := FReferences.Count;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
    WriteStrToStream(Stream, TSepiMeta(FReferences[I]).GetFullName);

  // Write locals information
  for I := 0 to MethodCount-1 do
    Methods[I].WriteLocalsInfo(Stream);
end;

{----------------------------}
{ TSepiMemoryReference class }
{----------------------------}

{*
  Crée une nouvelle référence mémoire
  @param AOptions     Options
  @param AConstSize   Taille de constante
*}
constructor TSepiMemoryReference.Create(AMethodAssembler: TSepiMethodAssembler;
  AOptions: TSepiAddressOptions = []; AConstSize: Integer = 0);
begin
  inherited Create;

  FMethodAssembler := AMethodAssembler;

  FOptions := AOptions;
  FConstSize := AConstSize;
  if FConstSize <= 0 then
    Exclude(FOptions, aoAcceptConstInCode);

  if [aoZeroAsNil, aoAcceptZero] * Options <> [] then
    FSpace := msZero
  else
    FSpace := msLocalsBase;
  FSpaceArgument := 0;

  if aoAcceptConstInCode in Options then
    GetMem(FConstant, ConstSize)
  else
    FConstant := nil;

  FSize := 0;
end;

{*
  [@inheritDoc]
*}
destructor TSepiMemoryReference.Destroy;
begin
  if Assigned(FConstant) then
    FreeMem(FConstant);
  ClearOperations;

  inherited;
end;

{*
  Nombre d'opérations
  @return Nombre d'opérations
*}
function TSepiMemoryReference.GetOperationCount: Integer;
begin
  Result := Length(FOperations);
end;

{*
  Tableau zero-based des opérations
  @param Index   Index d'une opération
  @return Opération à l'index spécifié
*}
function TSepiMemoryReference.GetOperations(
  Index: Integer): TSepiAddressDerefAndOpRec;
begin
  Result := FOperations[Index];
end;

{*
  Modifie l'espace mémoire
  Cette modification supprime toutes les opérations si le nouvel espace est
  mpZero (qui ne supporte pas les opérations).
  @param Value   Nouvelle valeur d'espace mémoire
*}
procedure TSepiMemoryReference.SetSpace(ASpace: TSepiMemorySpace;
  ASpaceArgument: Integer = 0);
begin
  case ASpace of
    msZero:
    begin
      if [aoZeroAsNil, aoAcceptZero] * Options = [] then
        raise ESepiInvalidMemoryReference.CreateRes(@SMemoryCantBeZero);
      ClearOperations;
    end;
    msConstant:
    begin
      if not (aoAcceptConstInCode in Options) then
        raise ESepiInvalidMemoryReference.CreateRes(@SMemoryCantBeConstant);
    end;
    msLocalsBase, msLocalsByte, msLocalsWord:
    begin
      case CardinalSize(ASpaceArgument, True) of
        0: ASpace := msLocalsBase;
        1: ASpace := msLocalsByte;
        2: ASpace := msLocalsWord;
      else
        raise ESepiInvalidMemoryReference.CreateRes(
          @SMemorySpaceOffsetMustBeWord);
      end;
    end;
    msParamsBase, msParamsByte, msParamsWord:
    begin
      case CardinalSize(ASpaceArgument, True) of
        0: ASpace := msParamsBase;
        1: ASpace := msParamsByte;
        2: ASpace := msParamsWord;
      else
        raise ESepiInvalidMemoryReference.CreateRes(
          @SMemorySpaceOffsetMustBeWord);
      end;
    end;
    msTrueConst:
    begin
      if not (aoAcceptTrueConst in Options) then
        raise ESepiInvalidMemoryReference.CreateRes(@SMemoryCantBeTrueConst);
    end;
  end;

  FSpace := ASpace;
  FSpaceArgument := ASpaceArgument;
end;

{*
  Modifie l'espace mémoire sur base d'un meta
  Le meta peut être une variable locale, une variable globale ou une constante
  globale (sous réserve d'acceptation des constantes).
  @param Meta   Meta à pointer par l'espace mémoire
*}
procedure TSepiMemoryReference.SetSpace(Meta: TSepiMeta);
begin
  if Meta is TSepiField then
  begin
    // Local variable

    if Meta.Owner <> MethodAssembler.Locals then
      raise ESepiInvalidMemoryReference.CreateResFmt(@SMemoryCantAccessObject,
        [Meta.GetFullName]);

    SetSpace(msLocalsBase, TSepiField(Meta).Offset);
  end else if Meta is TSepiConstant then
  begin
    // Global true constant
    with TSepiConstant(Meta) do
    begin
      if (not ConstType.NeedInit) and (ConstType.Size <= SizeOf(Extended)) then
      begin
        { Small constants which do not require initialization are directly
          written in the code. }
        SetSpace(msConstant);
        SetConstant(ValuePtr^);
      end else
      begin
        SetSpace(msTrueConst,
          MethodAssembler.UnitAssembler.MakeReference(Meta));
      end;
    end;
  end else if Meta is TSepiVariable then
  begin
    // Global variable or addressed constant
    SetSpace(msVariable, MethodAssembler.UnitAssembler.MakeReference(Meta));
  end else
  begin
    // Other types of meta are not accepted
    raise ESepiInvalidMemoryReference.CreateResFmt(@SMemoryCantAccessObject,
      [Meta.GetFullName]);
  end;
end;

{*
  Modifie l'espace mémoire sur base d'un paramètre
  Le paramètre doit être de la méthode courante. Si AutoDeref vaut True, et que
  le paramètre est passé par adresse, le déréférencement est ajouté
  automatiquement.
  @param Param       Paramètre à pointer par l'espace mémoire
  @param AutoDeref   Déréférence automatiquement
*}
procedure TSepiMemoryReference.SetSpace(Param: TSepiParam;
  AutoDeref: Boolean = True);
begin
  if Param.Owner <> MethodAssembler.SepiMethod.Signature then
    raise ESepiInvalidMemoryReference.CreateResFmt(@SMemoryCantAccessObject,
      [Param.Owner.Owner.GetFullName + '.' + Param.Name]);

  SetSpace(msParamsBase, Param.CallInfo.SepiStackOffset);

  if AutoDeref and Param.CallInfo.ByAddress then
    AddOperation(adSimple);
end;

{*
  Modifie l'espace mémoire sur base d'un nom, qui est d'abord recherché
  L'espace mémoire est recherché dans les variables locales, puis dans les
  paramètres, puis via la méthode LookFor de la méthode qui est asssemblée.
  Si AutoDerefParam vaut True, et que c'est un paramètre est passé par adresse,
  le déréférencement est ajouté automatiquement.
  @param Name             Nom de l'espace mémoire, à rechercher
  @param AutoDerefParam   Déréférence automatiquement les paramètres
*}
procedure TSepiMemoryReference.SetSpace(const Name: string;
  AutoDerefParam: Boolean = True);
var
  Obj: TObject;
begin
  Obj := MethodAssembler.LookFor(Name);
  if Obj is TSepiMeta then
    SetSpace(TSepiMeta(Obj))
  else if Obj is TSepiParam then
    SetSpace(TSepiParam(Obj), AutoDerefParam)
  else
    raise ESepiMetaNotFoundError.CreateResFmt(@SSepiObjectNotFound, [Name]);
end;

{*
  Assigne la référence mémoire à une constante entière
  @param Value   Valeur constante
*}
procedure TSepiMemoryReference.SetAsConst(Value: Int64);
begin
  if Value = 0 then
    SetSpace(msZero)
  else
  begin
    SetSpace(msConstant);
    SetConstant(Value);
  end;
end;

{*
  Assigne la référence mémoire à une constante booléenne
  @param Value   Valeur constante
*}
procedure TSepiMemoryReference.SetAsConst(Value: Boolean);
begin
  if not Value then
    SetSpace(msZero)
  else
  begin
    SetSpace(msConstant);
    SetConstant(Value);
  end;
end;

{*
  Assigne la référence mémoire à une constante flottante
  @param Value   Valeur constante
*}
procedure TSepiMemoryReference.SetAsConst(Value: Extended);
begin
  SetSpace(msConstant);
  case ConstSize of
    4: Single(FConstant^) := Value;
    8: Double(FConstant^) := Value;
  else
    SetConstant(Value);
  end;
end;

{*
  Assigne la référence mémoire à une constante Currency
  @param Value   Valeur constante
*}
procedure TSepiMemoryReference.SetAsConst(Value: Currency);
begin
  SetSpace(msConstant);
  SetConstant(Value);
end;

{*
  Supprime toutes les opérations
*}
procedure TSepiMemoryReference.ClearOperations;
var
  I: Integer;
begin
  for I := 0 to Length(FOperations)-1 do
    FOperations[I].MemOperationArg.Free;
  SetLength(FOperations, 0);
end;

{*
  Ajoute un déréférencement et une opération
  @param ADereference         Déréférencement
  @param AOperation           Opération
  @param AConstOperationArg   Argument constant de l'opération, si applicable
  @return Argument mémoire de l'opération, si applicable (nil sinon)
*}
function TSepiMemoryReference.AddOperation(
  ADereference: TSepiAddressDereference; AOperation: TSepiAddressOperation;
  AConstOperationArg: Integer = 0): TSepiMemoryReference;
const
  OnlyShortConstArgOps = [
    aoPlusConstTimesMemShortint, aoPlusConstTimesMemSmallint,
    aoPlusConstTimesMemLongint
  ];
  ConstArgSizeToOp: array[1..4] of TSepiAddressOperation = (
    aoPlusConstShortint, aoPlusConstSmallint, aoPlusConstLongint,
    aoPlusConstLongint
  );
  MemArgSizes: array[aoPlusMemShortint..aoPlusConstTimesMemLongint] of Integer
    = (1, 2, 4, 1, 2, 4);
var
  Index: Integer;
begin
  Index := Length(FOperations);

  // Check parameters consistency
  if (AOperation in OnlyShortConstArgOps) and
    (IntegerSize(AConstOperationArg) > 1) then
    raise ESepiInvalidMemoryReference.CreateRes(@SConstArgMustBeShort);

  // Try to compress dereference and operation
  if (Index > 0) and (ADereference = adNone) and
    (FOperations[Index-1].Operation = aoNone) then
  begin
    // Compression OK
    Dec(Index);
  end else
  begin
    // Some checks
    if Length(FOperations) >= MaxOperationCount then
      raise ESepiInvalidMemoryReference.CreateRes(@STooManyOperations);

    if Space = msZero then
      raise ESepiInvalidMemoryReference.CreateRes(
        @SZeroMemoryCantHaveOperations);

    // Add a new operation
    SetLength(FOperations, Index+1);

    // Set dereference
    FOperations[Index].Dereference := ADereference;
  end;

  // Set operation
  with FOperations[Index] do
  begin
    Operation := AOperation;
    ConstOperationArg := AConstOperationArg;

    // Adapt operation to const arg size
    if Operation in [aoPlusConstShortint..aoPlusConstLongint] then
      Operation := ConstArgSizeToOp[IntegerSize(AConstOperationArg)];

    // Create memory reference
    if Operation in OpsWithMemArg then
    begin
      MemOperationArg := TSepiMemoryReference.Create(MethodAssembler,
        aoAcceptAllConsts, MemArgSizes[Operation]);
    end else
      MemOperationArg := nil;

    Result := MemOperationArg;
  end;
end;

{*
  Ajoute un déréférencement
  @param ADereference   Déréférencement
*}
procedure TSepiMemoryReference.AddOperation(
  ADereference: TSepiAddressDereference);
begin
  AddOperation(ADereference, aoNone);
end;

{*
  Ajoute une opération
  @param AOperation           Opération
  @param AConstOperationArg   Argument constant de l'opération, si applicable
  @return Argument mémoire de l'opération, si applicable (nil sinon)
*}
function TSepiMemoryReference.AddOperation(AOperation: TSepiAddressOperation;
  AConstOperationArg: Integer = 0): TSepiMemoryReference;
begin
  Result := AddOperation(adNone, AOperation, AConstOperationArg);
end;

{*
  Récupère la constante
  La référence mémoire doit accepter les constantes non nulles.
  @param AConstant   En sortie : valeur de la constante
*}
procedure TSepiMemoryReference.GetConstant(var AConstant);
begin
  Move(FConstant^, AConstant, ConstSize);
end;

{*
  Spécifie la constante
  La référence mémoire doit accepter les constantes non nulles.
  @param AConstant   Nouvelle valeur de la constante
*}
procedure TSepiMemoryReference.SetConstant(const AConstant);
begin
  Move(AConstant, FConstant^, ConstSize);
end;

{*
  Construit la référence mémoire
*}
procedure TSepiMemoryReference.Make;
var
  I: Integer;
begin
  // Head byte (TSepiMemoryRef)
  FSize := SizeOf(TSepiMemoryRef);

  // Space argument
  case Space of
    msConstant:
      Inc(FSize, ConstSize);
    msLocalsByte, msParamsByte:
      Inc(FSize, SizeOf(Byte));
    msLocalsWord, msParamsWord:
      Inc(FSize, SizeOf(Word));
    msTrueConst, msVariable:
      Inc(FSize, SizeOf(Integer));
  end;

  // Operations
  for I := 0 to Length(FOperations)-1 do
  begin
    with FOperations[I] do
    begin
      // Head byte (TSepiAddressDerefAndOp)
      Inc(FSize, SizeOf(TSepiAddressDerefAndOp));

      // Const argument
      case Operation of
        aoPlusConstShortint, aoPlusConstTimesMemShortint,
          aoPlusConstTimesMemSmallint, aoPlusConstTimesMemLongint:
          Inc(FSize, SizeOf(Shortint));
        aoPlusConstSmallint:
          Inc(FSize, SizeOf(Smallint));
        aoPlusConstLongint:
          Inc(FSize, SizeOf(Longint));
      end;

      // Memory argument
      if Assigned(MemOperationArg) then
      begin
        MemOperationArg.Make;
        Inc(FSize, MemOperationArg.Size);
      end;
    end;
  end;
end;

{*
  Ecrit la référence mémoire dans un flux
  @param Stream   Flux de destination
*}
procedure TSepiMemoryReference.WriteToStream(Stream: TStream);
var
  MemRef: TSepiMemoryRef;
  I: Integer;
  DerefAndOp: TSepiAddressDerefAndOp;
begin
  // Head byte (TSepiMemoryRef)
  MemRef := MemoryRefEncode(Space, OperationCount);
  Stream.WriteBuffer(MemRef, SizeOf(TSepiMemoryRef));

  // Space argument
  case Space of
    msConstant:
      Stream.WriteBuffer(FConstant^, ConstSize);
    msLocalsByte, msParamsByte:
      Stream.WriteBuffer(FSpaceArgument, SizeOf(Byte));
    msLocalsWord, msParamsWord:
      Stream.WriteBuffer(FSpaceArgument, SizeOf(Word));
    msTrueConst, msVariable:
      Stream.WriteBuffer(FSpaceArgument, SizeOf(Integer));
  end;

  // Operations
  for I := 0 to Length(FOperations)-1 do
  begin
    with FOperations[I] do
    begin
      // Head byte (TSepiAddressDerefAndOp)
      DerefAndOp := AddressDerefAndOpEncode(Dereference, Operation);
      Stream.WriteBuffer(DerefAndOp, SizeOf(TSepiAddressDerefAndOp));

      // Const argument
      case Operation of
        aoPlusConstShortint, aoPlusConstTimesMemShortint,
          aoPlusConstTimesMemSmallint, aoPlusConstTimesMemLongint:
          Stream.WriteBuffer(ConstOperationArg, SizeOf(Shortint));
        aoPlusConstSmallint:
          Stream.WriteBuffer(ConstOperationArg, SizeOf(Smallint));
        aoPlusConstLongint:
          Stream.WriteBuffer(ConstOperationArg, SizeOf(Longint));
      end;

      // Memory argument
      if Assigned(MemOperationArg) then
        MemOperationArg.WriteToStream(Stream);
    end;
  end;
end;

{---------------------}
{ TSepiJumpDest class }
{---------------------}

{*
  Crée une destination de JUMP
  @param AMethodAssembler   Assembleur de méthode
  @param AAllowMemory       Autoriser ou non les destinations mémoire
*}
constructor TSepiJumpDest.Create(AMethodAssembler: TSepiMethodAssembler);
begin
  inherited Create;

  FMethodAssembler := AMethodAssembler;

  FLabelID := -1;
  FLabelName := '';
end;

{*
  Modifie l'ID du label
  @param Value   Nouvel ID du label
*}
procedure TSepiJumpDest.SetLabelID(Value: Integer);
begin
  if Value = FLabelID then
    Exit;

  FLabelID := Value;
  FLabelName := '';
end;

{*
  Modifie le nom du label
  @param Value   Nouveau nom du label
*}
procedure TSepiJumpDest.SetLabelName(const Value: string);
begin
  FLabelID := MethodAssembler.MakeLabel(Value);
  FLabelName := Value;
end;

{*
  Construit la destination de Jump
*}
procedure TSepiJumpDest.Make;
begin
end;

{*
  Calcule l'offset
  @param FromPos   Position de provenance
  @return Offset
*}
function TSepiJumpDest.MakeOffset(FromPos: Integer): Integer;
begin
  Result := MethodAssembler.FindLabel(LabelID) - FromPos;
end;

{*
  Ecrit la destination de JUMP dans un flux
  @param Stream    Flux de destination
  @param FromPos   Position de provenance
*}
procedure TSepiJumpDest.WriteToStream(Stream: TStream; FromPos: Integer);
var
  Offset: Smallint;
begin
  Offset := MakeOffset(FromPos);
  Stream.WriteBuffer(Offset, SizeOf(Smallint));
end;

{*
  Ecrit la destination de JUMP dans un flux
  @param Stream   Flux de destination
*}
procedure TSepiJumpDest.WriteToStream(Stream: TStream);
begin
  WriteToStream(Stream, Stream.Position + SizeOf(Smallint));
end;

end.

