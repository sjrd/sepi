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
  Compilateur Sepi
  @author sjrd
  @version 1.0
*}
unit SepiCompiler;

interface

uses
  SysUtils, Classes, Contnrs, ScUtils, SepiReflectionCore, SepiMembers,
  SepiOpCodes, SepiReflectionConsts;

resourcestring
  SLabelAlreadyExists = 'Le label ''%s'' existe déjà';
  SLabelNotFound = 'Label ''%s'' non trouvé';
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

type
  TSepiAsmInstrList = class;
  TSepiInstructionList = class;
  TSepiMethodCompiler = class;
  TSepiUnitCompiler = class;
  TSepiMemoryReference = class;

  {*
    Erreur de compilation Sepi
  *}
  ESepiCompilerError = class(Exception);

  {*
    Label non trouvé lors de la compilation Sepi
  *}
  ESepiLabelError = class(ESepiCompilerError);

  {*
    Référence mémoire invalide d'après l'instruction contenante
  *}
  ESepiInvalidMemoryReference = class(ESepiCompilerError);

  {*
    Destination de JUMP invalide
  *}
  ESepiInvalidJumpDest = class(ESepiCompilerError);

  {*
    Référence à une instruction
    @author sjrd
    @version 1.0
  *}
  TSepiInstructionRef = class(TObject)
  private
    FMethodCompiler: TSepiMethodCompiler; /// Compilateur

    FInstructionIndex: Integer; /// Position de l'instruction référencée

    procedure SetInstructionIndex(Value: Integer);

    function GetPosition: Integer;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    property MethodCompiler: TSepiMethodCompiler read FMethodCompiler;
    property InstructionIndex: Integer read FInstructionIndex;
    property Position: Integer read GetPosition;
  end;

  {*
    Instruction Sepi
    Les instructions Sepi sont toujours rattachées à un assembleur de méthodes.
    Lorsque l'assembleur de méthodes est libérés, toutes les instructions qui
    lui ont été rattachées sont libérées elles aussi. Ne libérez donc pas
    manuellement une instruction Sepi.
    @author sjrd
    @version 1.0
  *}
  TSepiInstruction = class(TObject)
  private
    FMethodCompiler: TSepiMethodCompiler; /// Compilateur
    FUnitCompiler: TSepiUnitCompiler;     /// Compilateur d'unité

    FBeforeRef: TSepiInstructionRef; /// Référence avant cette instruction
    FAfterRef: TSepiInstructionRef;  /// Référence après cette instruction
  protected
    procedure CustomCompile; virtual;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Compile;

    property MethodCompiler: TSepiMethodCompiler read FMethodCompiler;
    property UnitCompiler: TSepiUnitCompiler read FUnitCompiler;

    property BeforeRef: TSepiInstructionRef read FBeforeRef;
    property AfterRef: TSepiInstructionRef read FAfterRef;
  end;

  {*
    Instruction d'assemblage Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiAsmInstr = class(TSepiInstruction)
  private
    FPosition: Integer; /// Position de l'instruction
  protected
    FOpCode: TSepiOpCode; /// OpCode
    FSize: Integer;       /// Taille

    procedure CustomCompile; override;

    function GetEndPosition: Integer; virtual;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    procedure Make; virtual;
    procedure ComputeActualSize; virtual;
    procedure SetPosition(Value: Integer); virtual;
    procedure WriteToStream(Stream: TStream); virtual;

    property Position: Integer read FPosition;
    property OpCode: TSepiOpCode read FOpCode;
    property Size: Integer read FSize;
    property EndPosition: Integer read GetEndPosition;
  end;

  {*
    Liste d'instructions Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiInstructionList = class(TSepiInstruction)
  private
    FInstructions: TObjectList; /// Instructions

    function GetCount: Integer;
    function GetInstructions(Index: Integer): TSepiInstruction;
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    function Add(Instruction: TSepiInstruction): Integer;
    procedure Insert(Index: Integer; Instruction: TSepiInstruction);

    property Count: Integer read GetCount;
    property Instructions[Index: Integer]: TSepiInstruction
      read GetInstructions; default;
  end;

  {*
    Liste d'instructions assembleur Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiAsmInstrList = class(TObject)
  private
    FMethodCompiler: TSepiMethodCompiler; /// Compilateur

    FInstructions: TObjectList; /// Instructions

    FSize: Integer; /// Taille

    procedure Make;
    procedure ComputeActualSize;
    procedure SetPositions;

    function GetCount: Integer;
    function GetInstructions(Index: Integer): TSepiAsmInstr;
  protected
    procedure Add(Instruction: TSepiAsmInstr);

    property CurrentPos: Integer read GetCount;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);
    destructor Destroy; override;

    procedure Clear;

    procedure Assemble;
    procedure WriteToStream(Stream: TStream);

    property MethodCompiler: TSepiMethodCompiler read FMethodCompiler;

    property Count: Integer read GetCount;
    property Instructions[Index: Integer]: TSepiAsmInstr
      read GetInstructions; default;

    property Size: Integer read FSize;
  end;

  {*
    Pseudo-instruction label nommé
    @author sjrd
    @version 1.0
  *}
  TSepiNamedLabel = class(TSepiInstruction)
  private
    FName: string;
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
      const AName: string);

    procedure AfterConstruction; override;

    property Name: string read FName;
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
    Compilateur d'une méthode Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiMethodCompiler = class(TObject)
  private
    FUnitCompiler: TSepiUnitCompiler; /// Compilateur d'unité

    FObjFreeList: TObjectList; /// Liste des objets à libérer en fin de vie

    FSepiMethod: TSepiMethod; /// Méthode Sepi correspondante
    FLocals: TSepiRecordType; /// Variables locales

    FInstructions: TSepiInstructionList; /// Instructions
    FAsmInstructions: TSepiAsmInstrList; /// Instructions assembleur
    FSize: Integer;                      /// Taille totale (après assemblage)

    FLastInstruction: TSepiAsmInstrList;

    FNamedLabels: TStrings; /// Labels nommés (paire nom/instruction)

    FLocalsInfo: array of TLocalInfo; /// Informations sur les locales

    procedure SetLabel(NamedLabel: TSepiNamedLabel);
  protected
    property AsmInstructions: TSepiAsmInstrList read FAsmInstructions;
  public
    constructor Create(AUnitCompiler: TSepiUnitCompiler;
      ASepiMethod: TSepiMethod);
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AddObjToFreeList(Obj: TObject);

    function LookFor(const Name: string): TObject;

    function LabelExists(const LabelName: string): Boolean;
    function FindLabel(const LabelName: string;
      Create: Boolean = False): TSepiNamedLabel;

    procedure Compile;
    procedure WriteToStream(Stream: TStream);
    procedure WriteLocalsInfo(Stream: TStream);

    property UnitCompiler: TSepiUnitCompiler read FUnitCompiler;
    property SepiMethod: TSepiMethod read FSepiMethod;
    property Locals: TSepiRecordType read FLocals;

    property Instructions: TSepiInstructionList read FInstructions;
    property Size: Integer read FSize;
  end;

  {*
    Compilateur d'unité Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiUnitCompiler = class(TObject)
  private
    FSepiUnit: TSepiUnit;  /// Unité Sepi
    FMethods: TObjectList; /// Compilateurs de méthodes

    FReferences: TObjectList; /// Références

    function GetMethodCount: Integer;
    function GetMethods(Index: Integer): TSepiMethodCompiler;
  public
    constructor Create(ASepiUnit: TSepiUnit);
    destructor Destroy; override;

    function MakeReference(Meta: TSepiMeta): Integer;

    procedure WriteToStream(Stream: TStream);

    property SepiUnit: TSepiUnit read FSepiUnit;

    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TSepiMethodCompiler read GetMethods;
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
    FMethodCompiler: TSepiMethodCompiler; /// Compilateur de méthode

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
    constructor Create(AMethodCompiler: TSepiMethodCompiler;
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

    property MethodCompiler: TSepiMethodCompiler read FMethodCompiler;

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
    FMethodCompiler: TSepiMethodCompiler; /// Compilateur de méthode

    FInstructionRef: TSepiInstructionRef; /// Ref à l'instruction destination
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    procedure SetToLabel(NamedLabel: TSepiNamedLabel); overload;
    procedure SetToLabel(const LabelName: string;
      Create: Boolean = False); overload;

    procedure Make;

    function MakeOffset(FromPos: Integer): Integer;

    procedure WriteToStream(Stream: TStream; FromPos: Integer); overload;
    procedure WriteToStream(Stream: TStream); overload;

    property MethodCompiler: TSepiMethodCompiler read FMethodCompiler;

    property InstructionRef: TSepiInstructionRef
      read FInstructionRef write FInstructionRef;
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

{------------------------}
{ TSepiInstruction class }
{------------------------}

{*
  Crée une nouvelle instruction Sepi
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiInstruction.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create;

  FMethodCompiler := AMethodCompiler;
  FUnitCompiler := FMethodCompiler.UnitCompiler;

  FBeforeRef := TSepiInstructionRef.Create(MethodCompiler);
  FAfterRef := TSepiInstructionRef.Create(MethodCompiler);
end;

{*
  [@inheritDoc]
*}
destructor TSepiInstruction.Destroy;
begin
  FBeforeRef.Free;
  FAfterRef.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiInstruction.AfterConstruction;
begin
  inherited;

  MethodCompiler.AddObjToFreeList(Self);
end;

{*
  Compile l'instruction
  Cette méthode doit être surchargée par toutes les instructions afin de se
  compiler. La compilation fera intervenir la création d'instructions assembleur
  et leur compilation, en fin de récursion. La compilation des instructions
  assembleur est particulière et consiste uniquement à s'ajouter à la liste des
  instructions assembleur de la méthode.
  Pour compiler les instructions subalternes, appelez Compile et non
  CustomCompile. Mais surchargez CustomCompile.
*}
procedure TSepiInstruction.CustomCompile;
begin
end;

{*
  Compile l'instruction
*}
procedure TSepiInstruction.Compile;
begin
  FBeforeRef.SetInstructionIndex(MethodCompiler.FAsmInstructions.CurrentPos);

  CustomCompile;

  FAfterRef.SetInstructionIndex(MethodCompiler.FAsmInstructions.CurrentPos);
end;

{---------------------------}
{ TSepiInstructionRef class }
{---------------------------}

{*
  Crée une référence à une instruction
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiInstructionRef.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create;

  FMethodCompiler := AMethodCompiler;
  FInstructionIndex := -1;
end;

{*
  Renseigne la position de l'instruction référencée
  @param Value Position de l'instruction référencée
*}
procedure TSepiInstructionRef.SetInstructionIndex(Value: Integer);
begin
  Assert(FInstructionIndex < 0);
  FInstructionIndex := Value;
end;

{*
  Position de la référence
  @return Position de la référence
*}
function TSepiInstructionRef.GetPosition: Integer;
var
  Instructions: TSepiAsmInstrList;
begin
  Assert(FInstructionIndex >= 0);

  Instructions := MethodCompiler.AsmInstructions;

  if InstructionIndex < Instructions.Count then
    Result := Instructions[InstructionIndex].Position
  else
    Result := Instructions.Size;
end;

{---------------------}
{ TSepiAsmInstr class }
{---------------------}

{*
  Crée une nouvelle instruction assembleur Sepi
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmInstr.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FOpCode := ocNope;
end;

{*
  Compile l'instruction assembleur
  La compilation d'une instruction assembleur consiste uniquement à s'ajouter à
  la liste des instructions assembleur de son compilateur de méthode.
*}
procedure TSepiAsmInstr.CustomCompile;
begin
  MethodCompiler.FAsmInstructions.Add(Self);
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

{----------------------------}
{ TSepiInstructionList class }
{----------------------------}

{*
  Crée une liste d'instructions
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiInstructionList.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FInstructions := TObjectList.Create(False);
end;

{*
  [@inheritDoc]
*}
destructor TSepiInstructionList.Destroy;
begin
  FInstructions.Free;

  inherited;
end;

{*
  Nombre d'instructions
  @return Nombre d'instructions dans la liste
*}
function TSepiInstructionList.GetCount: Integer;
begin
  Result := FInstructions.Count;
end;

{*
  Tableau zero-based des instructions
  @param Index   Index d'une instruction
  @return L'instruction à l'index spécifié
*}
function TSepiInstructionList.GetInstructions(Index: Integer): TSepiInstruction;
begin
  Result := TSepiInstruction(FInstructions[Index]);
end;

{*
  Compile les instructions
*}
procedure TSepiInstructionList.CustomCompile;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Instructions[I].Compile;
end;

{*
  Ajoute une instruction à la fin de la liste
  @param Instruction   Instruction à ajouter
  @return Position de l'instruction dans la liste
*}
function TSepiInstructionList.Add(Instruction: TSepiInstruction): Integer;
begin
  Result := FInstructions.Add(Instruction);
end;

{*
  Insère une instruction à une position donnée dans la liste
  @param Index         Index où insérer l'instruction
  @param Instruction   Instruction à insérer
*}
procedure TSepiInstructionList.Insert(Index: Integer;
  Instruction: TSepiInstruction);
begin
  FInstructions.Insert(Index, Instruction);
end;

{-------------------------}
{ TSepiAsmInstrList class }
{-------------------------}

{*
  Crée une liste d'instructions assembleur
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiAsmInstrList.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create;

  FMethodCompiler := AMethodCompiler;
  FInstructions := TObjectList.Create(False);

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
procedure TSepiAsmInstrList.SetPositions;
var
  I, Pos: Integer;
begin
  Pos := 0;

  for I := 0 to Count-1 do
  begin
    Instructions[I].SetPosition(Pos);
    Inc(Pos, Instructions[I].Size);
  end;
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
  Ajoute une instruction assembleur à la liste
  @param Instruction Instruction à ajouter
*}
procedure TSepiAsmInstrList.Add(Instruction: TSepiAsmInstr);
begin
  FInstructions.Add(Instruction);
end;

{*
  Vide la liste des instructions
*}
procedure TSepiAsmInstrList.Clear;
begin
  FInstructions.Clear;
end;

{*
  Assemble les instructions
*}
procedure TSepiAsmInstrList.Assemble;
begin
  Make;
  SetPositions;
  ComputeActualSize;
  SetPositions;
end;

{*
  Ecrit les instructions dans un flux
  @param Stream   Flux destination
*}
procedure TSepiAsmInstrList.WriteToStream(Stream: TStream);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Instructions[I].WriteToStream(Stream);
end;

{-----------------------}
{ TSepiNamedLabel class }
{-----------------------}

{*
  Crée une nouveau label nommé
  @param AMethodCompiler   Compilateur de méthode
  @param AName             Nom du label
*}
constructor TSepiNamedLabel.Create(AMethodCompiler: TSepiMethodCompiler;
  const AName: string);
begin
  inherited Create(AMethodCompiler);

  FName := AName;
end;

{*
  [@inheritDoc]
*}
procedure TSepiNamedLabel.CustomCompile;
begin
end;

{*
  [@inheritDoc]
*}
procedure TSepiNamedLabel.AfterConstruction;
begin
  MethodCompiler.SetLabel(Self);
end;

{---------------------------}
{ TSepiMethodCompiler class }
{---------------------------}

{*
  Crée un nouveau compilateur de méthode Sepi
  Si la méthode possède déjà des variables locales - c'est-à-dire si un type
  record dont le nom est LocalVarsName est présent - celles-ci sont prises
  comme variables locales de compilation. Sinon, le compilateur crée un record
  de variables locales, et le libèrera à sa destruction.
  @param AUnitCompiler   Compilateur d'unité
  @param ASepiMethod     Méthode Sepi
*}
constructor TSepiMethodCompiler.Create(AUnitCompiler: TSepiUnitCompiler;
  ASepiMethod: TSepiMethod);
begin
  inherited Create;

  FUnitCompiler := AUnitCompiler;
  FSepiMethod := ASepiMethod;

  FObjFreeList := TObjectList.Create(False);

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

  FInstructions := TSepiInstructionList.Create(Self);
  FAsmInstructions := TSepiAsmInstrList.Create(Self);
  FLastInstruction := nil;
  FSize := 0;

  FNamedLabels := TStringList.Create;
  TStringList(FNamedLabels).CaseSensitive := False;
end;

{*
  [@inheritDoc]
*}
destructor TSepiMethodCompiler.Destroy;
begin
  FNamedLabels.Free;
  FAsmInstructions.Free;

  FObjFreeList.Free;

  inherited;
end;

{*
  Notifie l'existence d'un label nommé
  @param NamedLabel   Label nommé à ajouter
*}
procedure TSepiMethodCompiler.SetLabel(NamedLabel: TSepiNamedLabel);
var
  Index: Integer;
begin
  Index := FNamedLabels.IndexOf(NamedLabel.Name);
  
  if Index < 0 then
    FNamedLabels.AddObject(NamedLabel.Name, NamedLabel)
  else
    raise ESepiLabelError.CreateResFmt(@SLabelAlreadyExists, [NamedLabel.Name]);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodCompiler.AfterConstruction;
begin
  inherited;

  UnitCompiler.FMethods.Add(Self);
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodCompiler.BeforeDestruction;
var
  I: Integer;
begin
  inherited;

  for I := 0 to FObjFreeList.Count-1 do
    FObjFreeList[I].Free;
  FObjFreeList.Clear;
end;

{*
  Ajoute un objet à ceux devant être libérés en fin de vie
*}
procedure TSepiMethodCompiler.AddObjToFreeList(Obj: TObject);
begin
  if FObjFreeList.IndexOf(Obj) < 0 then
    FObjFreeList.Add(Obj);
end;

{*
  Cherche un objet à partir de son nom
  LookFor cherche parmi les variables locales, puis les paramètres, et enfin
  essaie la méthode LookFor de la méthode Sepi correspondante. Le résultat peut
  être de type TSepiMeta ou TSepiParam.
  @param Name   Nom de l'objet recherché
  @return Objet recherché, ou nil si non trouvé
*}
function TSepiMethodCompiler.LookFor(const Name: string): TObject;
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
  Teste l'existence d'un un label nommé
  @param LabelName   Nom du label
  @return True si un label de ce nom existe, False sinon
*}
function TSepiMethodCompiler.LabelExists(const LabelName: string): Boolean;
begin
  Result := FNamedLabels.IndexOf(LabelName) >= 0;
end;

{*
  Cherche un label nommé
  @param LabelName   Nom du label recherché
  @param Create      Si True, un label non trouvé est créé automatiquement
  @return Label nommé
  @throws ESepiLabelError Le label n'a pas été trouvé
*}
function TSepiMethodCompiler.FindLabel(
  const LabelName: string; Create: Boolean = False): TSepiNamedLabel;
var
  Index: Integer;
begin
  Index := FNamedLabels.IndexOf(LabelName);

  if Index >= 0 then
    Result := TSepiNamedLabel(FNamedLabels.Objects[Index])
  else if Create then
    Result := TSepiNamedLabel.Create(Self, LabelName)
  else
    raise ESepiLabelError.CreateResFmt(@SLabelNotFound, [LabelName]);
end;

{*
  Compile les instructions
*}
procedure TSepiMethodCompiler.Compile;
var
  Count: Integer;
  I: Integer;
  LocalVar: TSepiField;
begin
  // Local variables must have been completed
  if not Locals.Completed then
    Locals.Complete;

  // Compile
  AsmInstructions.Clear;
  Instructions.Compile;

  // Assemble
  AsmInstructions.Assemble;
  FSize := AsmInstructions.Size;

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
      TypeRef := UnitCompiler.MakeReference(LocalVar.FieldType);
      Offset := LocalVar.Offset;
      Inc(Count);
    end;
  end;
  SetLength(FLocalsInfo, Count);
end;

{*
  Ecrit la méthode dans un flux (tel que TSepiRuntimeMethod puisse le lire)
  La méthode doit avoir été compilée au préalable via la méthode Compile.
  @param Stream   Flux de destination
*}
procedure TSepiMethodCompiler.WriteToStream(Stream: TStream);
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
  AsmInstructions.WriteToStream(Stream);
end;

{*
  Ecrit les informations d'initialisation des variables locales
  @param Stream   Flux de destination
*}
procedure TSepiMethodCompiler.WriteLocalsInfo(Stream: TStream);
var
  Count: Integer;
begin
  Count := Length(FLocalsInfo);
  Stream.WriteBuffer(Count, 4);

  Stream.WriteBuffer(FLocalsInfo[0], Count*SizeOf(TLocalInfo));
end;

{-------------------------}
{ TSepiUnitCompiler class }
{-------------------------}

{*
  Crée un nouveau compilateur d'unité Sepi
  @param ASepiUnit   Unité Sepi à assembler
*}
constructor TSepiUnitCompiler.Create(ASepiUnit: TSepiUnit);
begin
  inherited Create;

  FSepiUnit := ASepiUnit;
  FMethods := TObjectList.Create;
  FReferences := TObjectList.Create(False);
end;

{*
  [@inheritDoc]
*}
destructor TSepiUnitCompiler.Destroy;
begin
  FReferences.Free;
  FMethods.Free;

  inherited;
end;

{*
  Nombre de compilateurs de méthode
  @return Nombre de compilateurs de méthode
*}
function TSepiUnitCompiler.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

{*
  Tableau zero-based des compilateurs de méthode
  @param Index   Index d'un compilateur
  @return Compilateur à l'index spécifié
*}
function TSepiUnitCompiler.GetMethods(Index: Integer): TSepiMethodCompiler;
begin
  Result := TSepiMethodCompiler(FMethods[Index]);
end;

{*
  Construit un numéro de référence à un meta Sepi
  @param Meta   Meta pour lequel construire un numéro de référence
  @return Numéro de référence du meta
*}
function TSepiUnitCompiler.MakeReference(Meta: TSepiMeta): Integer;
begin
  Result := FReferences.IndexOf(Meta);
  if Result < 0 then
    Result := FReferences.Add(Meta);
end;

{*
  Ecrit l'unité compilée dans un flux
  @param Stream   Flux destination
*}
procedure TSepiUnitCompiler.WriteToStream(Stream: TStream);
var
  I, Count: Integer;
begin
  // Compile methods
  for I := 0 to MethodCount-1 do
    Methods[I].Compile;

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
  @param AMethodCompiler   Compilateur de méthode
  @param AOptions          Options
  @param AConstSize        Taille de constante
*}
constructor TSepiMemoryReference.Create(AMethodCompiler: TSepiMethodCompiler;
  AOptions: TSepiAddressOptions = []; AConstSize: Integer = 0);
begin
  inherited Create;

  FMethodCompiler := AMethodCompiler;

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
  msZero (qui ne supporte pas les opérations).
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

    if Meta.Owner <> MethodCompiler.Locals then
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
          MethodCompiler.UnitCompiler.MakeReference(Meta));
      end;
    end;
  end else if Meta is TSepiVariable then
  begin
    // Global variable or addressed constant
    SetSpace(msVariable, MethodCompiler.UnitCompiler.MakeReference(Meta));
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
  if Param.Owner <> MethodCompiler.SepiMethod.Signature then
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
  Obj := MethodCompiler.LookFor(Name);
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
      MemOperationArg := TSepiMemoryReference.Create(MethodCompiler,
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
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiJumpDest.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create;

  FMethodCompiler := AMethodCompiler;

  FInstructionRef := nil;
end;

{*
  Position la destination du Jump sur un label nommé
  @param NamedLabel   Label nommé
*}
procedure TSepiJumpDest.SetToLabel(NamedLabel: TSepiNamedLabel);
begin
  InstructionRef := NamedLabel.BeforeRef;
end;

{*
  Position la destination du Jump sur un label nommé
  @param LabelName   Nom du label
*}
procedure TSepiJumpDest.SetToLabel(const LabelName: string;
  Create: Boolean = False);
begin
  SetToLabel(MethodCompiler.FindLabel(LabelName, Create));
end;

{*
  Construit la destination de Jump
*}
procedure TSepiJumpDest.Make;
begin
  Assert(InstructionRef <> nil);
end;

{*
  Calcule l'offset
  @param FromPos   Position de provenance
  @return Offset
*}
function TSepiJumpDest.MakeOffset(FromPos: Integer): Integer;
begin
  Result := InstructionRef.Position - FromPos;
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

