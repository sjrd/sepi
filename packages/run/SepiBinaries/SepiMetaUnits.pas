unit SepiMetaUnits;

interface

uses
  SysUtils, Classes, Contnrs, SepiCore, ScUtils, IniFiles, ScLists,
  SepiBinariesConsts;

type
  TSepiTypeKind = (tkInteger, tkInt64, tkDouble, tkString, tkBoolean, tkEnum,
    tkSet, tkRecord, tkClass, tkObject, tkArray, tkDynArray, tkMethodRef,
    tkDelegate);
  TSepiMemberVisibility = (mvPrivate, mvInternal, mvProtected,
    mvInternalProtected, mvPublic, mvPublished);
  TSepiParamKind = (pkInParam, pkOutParam, pkInOutParam, pkConstant);
  TSepiMethodKind = (mkNormal, mkConstructor, mkDestructor);
  TSepiMethodOverrideState = (osNone, osVirtual, osDynamic, osOverride);
  TSepiCallConvention = (ccRegister, ccStdCall, ccPascal, ccCDecl);
  TSepiPropertyAccessKind = (pakNone, pakVariable, pakMethod);

  ESepiMetaNotFoundError = class(ESepiError);

  TSepiMeta = class;
  TSepiMetaRoot = class;
  TSepiMetaUnit = class;
  TSepiMetaVariable = class;
  TSepiMetaMethod = class;
  TSepiMetaOverloadedMethod = class;
  TSepiMetaProperty = class;
  TSepiConstant = class;
  TSepiTypeAlias = class;

  TSepiMetaList = class(THashedStringList)
  private
    function GetMetas(Index : integer) : TSepiMeta;
    procedure SetMetas(Index : integer; Value : TSepiMeta);
    function GetMetaFromName(Name : string) : TSepiMeta;
    procedure SetMetaFromName(Name : string; Value : TSepiMeta);
  public
    constructor Create;

    function AddMeta(Meta : TSepiMeta) : integer;
    function IndexOfMeta(Meta : TSepiMeta) : integer;
    function Remove(Meta : TSepiMeta) : integer;

    property Metas[index : integer] : TSepiMeta read GetMetas write SetMetas;
    property MetaFromName[Name : string] : TSepiMeta
      read GetMetaFromName write SetMetaFromName;
  end;

  TSepiMeta = class
  private
    FLoading : boolean;
    FDestroying : boolean;
    FOwner : TSepiMeta;
    FRoot : TSepiMetaRoot;
    FOwningUnit : TSepiMetaUnit;
    FName : string;
    FVisibility : TSepiMemberVisibility;
    FChildren : TSepiMetaList;

    procedure AddChild(Child : TSepiMeta);
    procedure RemoveChild(Child : TSepiMeta);
  protected
    procedure ChildAdded(Child : TSepiMeta); virtual;
    procedure ChildRemoving(Child : TSepiMeta); virtual;

    procedure Loaded; virtual;

    property Loading : boolean read FLoading;
    property Destroying : boolean read FDestroying;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); virtual;
    constructor Create(AOwner : TSepiMeta; AName : string);
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function GetFullName : string;
    function GetMeta(Name : string) : TSepiMeta;
    function FindMeta(Name : string) : TSepiMeta;

    property Owner : TSepiMeta read FOwner;
    property Root : TSepiMetaRoot read FRoot;
    property OwningUnit : TSepiMetaUnit read FOwningUnit;
    property Name : string read FName;
    property Visibility : TSepiMemberVisibility
      read FVisibility write FVisibility;
  end;

  TSepiType = class(TSepiMeta)
  protected
    FKind : TSepiTypeKind;
  public
    class function LoadKind(AOwner : TSepiMeta; Stream : TStream) : TSepiType;

    function CompatibleWith(AType : TSepiType) : boolean; virtual;

    property Kind : TSepiTypeKind read FKind;
  end;

  TSepiMetaVariableContainer = class(TSepiType)
  private
    FVariables : TSepiMetaList;

    function GetVariableCount : integer;
    function GetVariables(Index : integer) : TSepiMetaVariable;
  protected
    procedure ChildAdded(Child : TSepiMeta); override;
    procedure ChildRemoving(Child : TSepiMeta); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);
    destructor Destroy; override;

    property VariableCount : integer read GetVariableCount;
    property Variables[index : integer] : TSepiMetaVariable read GetVariables;
  end;

  TSepiMetaMemberContainer = class(TSepiMetaVariableContainer)
  private
    FMethods : TSepiMetaList;
    FOverloadedMethods : TSepiMetaList;
    FProperties : TSepiMetaList;

    function GetMethodCount : integer;
    function GetMethods(Index : integer) : TSepiMetaMethod;
    function GetOverloadedMethodCount : integer;
    function GetOverloadedMethods(Index : integer) : TSepiMetaOverloadedMethod;
    function GetPropertyCount : integer;
    function GetProperties(Index : integer) : TSepiMetaProperty;
  protected
    procedure ChildAdded(Child : TSepiMeta); override;
    procedure ChildRemoving(Child : TSepiMeta); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);
    destructor Destroy; override;

    property MethodCount : integer read GetMethodCount;
    property Methods[index : integer] : TSepiMetaMethod read GetMethods;
    property OverloadedMethodCount : integer read GetOverloadedMethodCount;
    property OverloadedMethods[index : integer] : TSepiMetaOverloadedMethod
      read GetOverloadedMethods;
    property PropertyCount : integer read GetPropertyCount;
    property Properties[index : integer] : TSepiMetaProperty read GetProperties;
  end;

  TSepiMetaOmniContainer = class(TSepiMetaMemberContainer)
  private
    FTypes : TSepiMetaList;
    FConstants : TSepiMetaList;
    FTypeAliases : TSepiMetaList;

    function GetTypeCount : integer;
    function GetTypes(Index : integer) : TSepiType;
    function GetConstantCount : integer;
    function GetConstants(Index : integer) : TSepiConstant;
    function GetTypeAliasCount : integer;
    function GetTypeAliases(Index : integer) : TSepiTypeAlias;
  protected
    procedure ChildAdded(Child : TSepiMeta); override;
    procedure ChildRemoving(Child : TSepiMeta); override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);
    destructor Destroy; override;

    property TypeCount : integer read GetTypeCount;
    property Types[index : integer] : TSepiType read GetTypes;
    property ConstantCount : integer read GetConstantCount;
    property Constants[index : integer] : TSepiConstant read GetConstants;
    property TypeAliasCount : integer read GetTypeAliasCount;
    property TypeAliases[index : integer] : TSepiTypeAlias read GetTypeAliases;
  end;

  TSepiMetaRoot = class(TSepiMeta)
  private
    function GetUnitCount : integer;
    function GetUnits(Index : integer) : TSepiMetaUnit;
  public
    constructor Create;

    procedure LoadUnit(UnitName : string);

    property UnitCount : integer read GetUnitCount;
    property Units[index : integer] : TSepiMetaUnit read GetUnits;
  end;

  TSepiMetaUnit = class(TSepiMetaOmniContainer)
  private
    { TODO 2 -cMetaunit�s : Ajouter des champs concernant les en-t�te d'unit� }
    FCurrentVisibility : TSepiMemberVisibility;
    FLoadingReferences : TStrings;

    procedure SetCurrentVisibility(Value : TSepiMemberVisibility);
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);

    procedure LoadRef(var Ref);

    property CurrentVisibility : TSepiMemberVisibility
      read FCurrentVisibility write SetCurrentVisibility;
  end;

  TSepiTypeAlias = class(TSepiMeta)
  private
    FDest : TSepiType;
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      ADest : TSepiType);

    property Dest : TSepiType read FDest;
  end;

  TSepiConstant = class(TSepiMeta)
  private
    FType : TSepiType;
    FValue : TMemoryStream;
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      AType : TSepiType);

    property ConstType : TSepiType read FType;
    property Value : TMemoryStream read FValue;
  end;

  TSepiMetaVariable = class(TSepiMeta)
  private
    FType : TSepiType;
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      AType : TSepiType);

    property VarType : TSepiType read FType;
  end;

  TSepiMetaParam = class(TSepiMetaVariable)
  private
    FParamKind : TSepiParamKind;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      AType : TSepiType; AParamKind : TSepiParamKind = pkInParam);

    function CompatibleWith(AVariable : TSepiMetaVariable) : boolean;

    property ParamKind : TSepiParamKind read FParamKind;
  end;

  TSepiMethodSignature = class
  private
    FOwner : TSepiMeta;
    FParams : TSepiMetaList;
    FReturnType : TSepiType;

    procedure Loaded;

    function GetParamCount : integer;
    function GetParams(Index : integer) : TSepiMetaParam;
  public
    constructor Create(AOwner : TSepiMeta);
    destructor Destroy; override;

    procedure Load(Stream : TStream);

    function CompatibleWith(ASignature : TSepiMethodSignature) : boolean;

    property ParamCount : integer read GetParamCount;
    property Params[index : integer] : TSepiMetaParam read GetParams;
    property ReturnType : TSepiType read FReturnType;
  end;

  TSepiMetaMethod = class(TSepiMeta)
  private
    { TODO 2 -cMetaunit�s : Ajouter des champs concernant les directives de m�thodes }
    FSignature : TSepiMethodSignature;
    FKind : TSepiMethodKind;
    FOverrideState : TSepiMethodOverrideState;
    FAbstract : boolean;
    FInherited : TSepiMetaMethod;

    procedure FindInherited;
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      AKind : TSepiMethodKind = mkNormal;
      AOverrideState : TSepiMethodOverrideState = osNone;
      AAbstract : boolean = False);
    destructor Destroy; override;

    property Signature : TSepiMethodSignature read FSignature;
    property OverrideState : TSepiMethodOverrideState read FOverrideState;
    property IsAbstract : boolean read FAbstract;
  end;

  TSepiMetaSepiMethod = class(TSepiMetaMethod)
  private
    FVariables : TSepiMetaList;
    FCode : TStream;

    function GetVariableCount : integer;
    function GetVariables(Index : integer) : TSepiMetaVariable;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      AKind : TSepiMethodKind = mkNormal;
      AOverrideState : TSepiMethodOverrideState = osNone;
      AAbstract : boolean = False);
    destructor Destroy; override;

    property VariableCount : integer read GetVariableCount;
    property Variables[index : integer] : TSepiMetaVariable read GetVariables;
    property Code : TStream read FCode;
  end;

  TSepiMetaDelphiMethod = class(TSepiMetaMethod)
  private
    FCallConvention : TSepiCallConvention;
    FCode : Pointer;
  public
    constructor Create(AOwner : TSepiMeta; AName : string;
      ACode : Pointer; AKind : TSepiMethodKind = mkNormal;
      AOverrideState : TSepiMethodOverrideState = osNone;
      AAbstract : boolean = False;
      ACallConvention : TSepiCallConvention = ccRegister);

    property CallConvention : TSepiCallConvention read FCallConvention;
    property Code : Pointer read FCode;
  end;

  TSepiMetaOverloadedMethod = class(TSepiMeta)
  private
    FMethodCount : integer;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string);

    function NextID : integer;
    function FindMethod(Signature : TSepiMethodSignature) : TSepiMetaMethod;
  end;

  TSepiMetaProperty = class(TSepiMeta)
  private
    FSignature : TSepiMethodSignature;

    FReadAccess : TSepiMeta;
    FReadAccessKind : TSepiPropertyAccessKind;
    FReadVariableAccess : TSepiMetaVariable;
    FReadMethodAccess : TSepiMetaMethod;

    FWriteAccess : TSepiMeta;
    FWriteAccessKind : TSepiPropertyAccessKind;
    FWriteVariableAccess : TSepiMetaVariable;
    FWriteMethodAccess : TSepiMetaMethod;

    procedure DevelopAccess;
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; AName : string;
      AReadAccess : TSepiMeta; AWriteAccess : TSepiMeta);
    destructor Destroy; override;

    property Signature : TSepiMethodSignature read FSignature;

    property ReadAccessKind : TSepiPropertyAccessKind read FReadAccessKind;
    property ReadVariableAccess : TSepiMetaVariable read FReadVariableAccess;
    property ReadMethodAccess : TSepiMetaMethod read FReadMethodAccess;

    property WriteAccessKind : TSepiPropertyAccessKind read FWriteAccessKind;
    property WriteVariableAccess : TSepiMetaVariable read FWriteVariableAccess;
    property WriteMethodKind : TSepiMetaMethod read FWriteMethodAccess;
  end;

  TSepiImportUnitFunc = function(Root : TSepiMetaRoot) : TSepiMetaUnit;

procedure SepiRegisterImportedUnit(const UnitName : string;
  ImportFunc : TSepiImportUnitFunc);

implementation

uses
  SepiTypes, SepiImportsSystem;

var
  SepiImportedUnits : TStrings;

procedure SepiRegisterImportedUnit(const UnitName : string;
  ImportFunc : TSepiImportUnitFunc);
begin
  SepiImportedUnits.AddObject(UnitName, TObject(@ImportFunc));
end;

function SepiImportedUnit(const UnitName : string) : TSepiImportUnitFunc;
var Index : integer;
begin
  Index := SepiImportedUnits.IndexOf(UnitName);
  if Index < 0 then Result := nil else
    Result := TSepiImportUnitFunc(SepiImportedUnits.Objects[Index]);
end;

////////////////////////////
/// Classe TSepiMetaList ///
////////////////////////////

constructor TSepiMetaList.Create;
begin
  inherited;
  CaseSensitive := True;
  Duplicates := dupError;
end;

function TSepiMetaList.GetMetas(Index : integer) : TSepiMeta;
begin
  Result := TSepiMeta(Objects[Index]);
end;

procedure TSepiMetaList.SetMetas(Index : integer; Value : TSepiMeta);
begin
  if Assigned(Objects[Index]) then
    Error(@SSepiMetaAlreadyAssigned, Index);
  Objects[Index] := Value;
end;

function TSepiMetaList.GetMetaFromName(Name : string) : TSepiMeta;
var Index : integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then Result := nil else
    Result := TSepiMeta(Objects[Index]);
end;

procedure TSepiMetaList.SetMetaFromName(Name : string; Value : TSepiMeta);
var Index : integer;
begin
  Index := IndexOf(Name);
  if Index < 0 then AddObject(Name, Value) else
    Metas[Index] := Value;
end;

function TSepiMetaList.AddMeta(Meta : TSepiMeta) : integer;
begin
  Result := AddObject(Meta.Name, Meta);
end;

function TSepiMetaList.IndexOfMeta(Meta : TSepiMeta) : integer;
begin
  Result := IndexOf(Meta.Name);
end;

function TSepiMetaList.Remove(Meta : TSepiMeta) : integer;
begin
  Result := IndexOfMeta(Meta);
  Delete(Result);
end;

////////////////////////
/// Classe TSepiMeta ///
////////////////////////

constructor TSepiMeta.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  Create(AOwner, ReadStrFromStream(Stream));
  Stream.ReadBuffer(FVisibility, 1);
  FLoading := True;
end;

constructor TSepiMeta.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create;
  FLoading := False;
  FDestroying := False;
  FOwner := AOwner;
  FName := AName;
  if Assigned(FOwner) then
  begin
    FRoot := FOwner.Root;
    FOwningUnit := FOwner.OwningUnit;
    FOwner.AddChild(Self);
  end else
  begin
    FRoot := nil;
    FOwningUnit := nil;
  end;
  FVisibility := mvPublic;
  FChildren := TSepiMetaList.Create;
end;

destructor TSepiMeta.Destroy;
var I : integer;
begin
  for I := 0 to FChildren.Count-1 do
    FChildren.Objects[I].Free; // a bit faster than using Metas[I]
  FChildren.Free;
  FOwner.RemoveChild(Self);
  inherited Destroy;
end;

procedure TSepiMeta.AddChild(Child : TSepiMeta);
begin
  FChildren.AddMeta(Child);
  ChildAdded(Child);
end;

procedure TSepiMeta.RemoveChild(Child : TSepiMeta);
begin
  if Destroying then exit;
  ChildRemoving(Child);
  FChildren.Remove(Child);
end;

procedure TSepiMeta.ChildAdded(Child : TSepiMeta);
begin
end;

procedure TSepiMeta.ChildRemoving(Child : TSepiMeta);
begin
end;

procedure TSepiMeta.Loaded;
var I : integer;
begin
  for I := 0 to FChildren.Count do
    FChildren.Metas[I].Loaded;
  FLoading := False;
end;

procedure TSepiMeta.BeforeDestruction;
begin
  inherited;
  FDestroying := True;
end;

function TSepiMeta.GetFullName : string;
begin
  if Assigned(FOwner) and (FOwner.Name <> '') then
    Result := FOwner.GetFullName+'.'+Name
  else
    Result := Name;
end;

function TSepiMeta.GetMeta(Name : string) : TSepiMeta;
var I : integer;
    Field : string;
begin
  I := Pos('.', Name);
  if I > 0 then
  begin
    Field := Copy(Name, I+1, Length(Name));
    Delete(Name, I, Length(Name));
  end else Field := '';

  Result := FChildren.MetaFromName[Name];

  if not Assigned(Result) then exit;
  while Result is TSepiTypeAlias do
    Result := TSepiTypeAlias(Result).Dest;
  if Field <> '' then
    Result := Result.GetMeta(Field);
end;

function TSepiMeta.FindMeta(Name : string) : TSepiMeta;
begin
  Result := GetMeta(Name);
  if Result = nil then
    raise ESepiMetaNotFoundError.CreateFmt(SSepiObjectNotFound, [Name]);
end;

////////////////////////
/// Classe TSepiType ///
////////////////////////

class function TSepiType.LoadKind(AOwner : TSepiMeta; Stream : TStream) : TSepiType;
var Kind : TSepiTypeKind;
begin
  Stream.ReadBuffer(Kind, 1);
  case Kind of
    tkInteger   : Result := TSepiIntegerType  .Load(AOwner, Stream);
    tkInt64     : Result := TSepiInt64Type    .Load(AOwner, Stream);
    tkDouble    : Result := TSepiDoubleType   .Load(AOwner, Stream);
    tkString    : Result := TSepiStringType   .Load(AOwner, Stream);
    tkBoolean   : Result := TSepiBooleanType  .Load(AOwner, Stream);
    tkEnum      : Result := TSepiEnumType     .Load(AOwner, Stream);
    tkSet       : Result := TSepiSetType      .Load(AOwner, Stream);
    tkRecord    : Result := TSepiRecordType   .Load(AOwner, Stream);
    tkClass     : Result := TSepiClassType    .Load(AOwner, Stream);
    tkObject    : Result := TSepiObjectType   .Load(AOwner, Stream);
    tkArray     : Result := TSepiArrayType    .Load(AOwner, Stream);
    tkDynArray  : Result := TSepiDynArrayType .Load(AOwner, Stream);
    tkMethodRef : Result := TSepiMethodRefType.Load(AOwner, Stream);
    tkDelegate  : Result := TSepiDelegateType .Load(AOwner, Stream);
    else Result := nil;
  end;
end;

function TSepiType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := AType.FKind = FKind;
end;

/////////////////////////////////////////
/// Classe TSepiMetaVariableContainer ///
/////////////////////////////////////////

constructor TSepiMetaVariableContainer.Load(AOwner : TSepiMeta; Stream : TStream);
var Count : integer;
begin
  inherited;
  FVariables := TSepiMetaList.Create;

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    TSepiMetaVariable.Load(Self, Stream);
    dec(Count);
  end;
end;

constructor TSepiMetaVariableContainer.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FVariables := TSepiMetaList.Create;
end;

destructor TSepiMetaVariableContainer.Destroy;
begin
  FVariables.Free;
  inherited;
end;

function TSepiMetaVariableContainer.GetVariableCount : integer;
begin
  Result := FVariables.Count;
end;

function TSepiMetaVariableContainer.GetVariables(Index : integer) : TSepiMetaVariable;
begin
  Result := TSepiMetaVariable(FVariables.Objects[Index]);
end;

procedure TSepiMetaVariableContainer.ChildAdded(Child : TSepiMeta);
begin
  inherited;
  if Child is TSepiMetaVariable then
    FVariables.AddMeta(Child);
end;

procedure TSepiMetaVariableContainer.ChildRemoving(Child : TSepiMeta);
begin
  if Child is TSepiMetaVariable then
    FVariables.Remove(Child);
  inherited;
end;

///////////////////////////////////////
/// Classe TSepiMetaMemberContainer ///
///////////////////////////////////////

constructor TSepiMetaMemberContainer.Load(AOwner : TSepiMeta; Stream : TStream);
var Count : integer;
begin
  inherited;
  FMethods := TSepiMetaList.Create;
  FOverloadedMethods := TSepiMetaList.Create;
  FProperties := TSepiMetaList.Create;

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    TSepiMetaMethod.Load(Self, Stream);
    dec(Count);
  end;

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    TSepiMetaOverloadedMethod.Load(Self, Stream);
    dec(Count);
  end;

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    TSepiMetaProperty.Load(Self, Stream);
    dec(Count);
  end;
end;

constructor TSepiMetaMemberContainer.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FMethods := TSepiMetaList.Create;
  FOverloadedMethods := TSepiMetaList.Create;
  FProperties := TSepiMetaList.Create;
end;

destructor TSepiMetaMemberContainer.Destroy;
begin
  FProperties.Free;
  FOverloadedMethods.Free;
  FMethods.Free;
  inherited;
end;

function TSepiMetaMemberContainer.GetMethodCount : integer;
begin
  Result := FMethods.Count;
end;

function TSepiMetaMemberContainer.GetMethods(Index : integer) : TSepiMetaMethod;
begin
  Result := TSepiMetaMethod(FMethods.Objects[Index]);
end;

function TSepiMetaMemberContainer.GetOverloadedMethodCount : integer;
begin
  Result := FOverloadedMethods.Count;
end;

function TSepiMetaMemberContainer.GetOverloadedMethods(Index : integer) : TSepiMetaOverloadedMethod;
begin
  Result := TSepiMetaOverloadedMethod(FOverloadedMethods.Objects[Index]);
end;

function TSepiMetaMemberContainer.GetPropertyCount : integer;
begin
  Result := FProperties.Count;
end;

function TSepiMetaMemberContainer.GetProperties(Index : integer) : TSepiMetaProperty;
begin
  Result := TSepiMetaProperty(FProperties.Objects[Index]);
end;

procedure TSepiMetaMemberContainer.ChildAdded(Child : TSepiMeta);
begin
  inherited;
  if Child is TSepiMetaMethod then
    FMethods.AddMeta(Child);
  if Child is TSepiMetaOverloadedMethod then
    FOverloadedMethods.AddMeta(Child);
  if Child is TSepiMetaProperty then
    FProperties.AddMeta(Child);
end;

procedure TSepiMetaMemberContainer.ChildRemoving(Child : TSepiMeta);
begin
  if Child is TSepiMetaProperty then
    FProperties.Remove(Child);
  if Child is TSepiMetaOverloadedMethod then
    FOverloadedMethods.Remove(Child);
  if Child is TSepiMetaMethod then
    FMethods.Remove(Child);
  inherited;
end;

/////////////////////////////////////
/// Classe TSepiMetaOmniContainer ///
/////////////////////////////////////

constructor TSepiMetaOmniContainer.Load(AOwner : TSepiMeta; Stream : TStream);
var Count : integer;
begin
  inherited;
  FTypes := TSepiMetaList.Create;
  FConstants := TSepiMetaList.Create;
  FTypeAliases := TSepiMetaList.Create;

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    TSepiType.LoadKind(Self, Stream);
    dec(Count);
  end;

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    TSepiConstant.Load(Self, Stream);
    dec(Count);
  end;

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    TSepiTypeAlias.Load(Self, Stream);
    dec(Count);
  end;
end;

constructor TSepiMetaOmniContainer.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FTypes := TSepiMetaList.Create;
  FConstants := TSepiMetaList.Create;
  FTypeAliases := TSepiMetaList.Create;
end;

destructor TSepiMetaOmniContainer.Destroy;
begin
  FTypeAliases.Free;
  FConstants.Free;
  FTypes.Free;
  inherited;
end;

function TSepiMetaOmniContainer.GetTypeCount : integer;
begin
  Result := FTypes.Count;
end;

function TSepiMetaOmniContainer.GetTypes(Index : integer) : TSepiType;
begin
  Result := TSepiType(FTypes.Objects[Index]);
end;

function TSepiMetaOmniContainer.GetConstantCount : integer;
begin
  Result := FConstants.Count;
end;

function TSepiMetaOmniContainer.GetConstants(Index : integer) : TSepiConstant;
begin
  Result := TSepiConstant(FConstants.Objects[Index]);
end;

function TSepiMetaOmniContainer.GetTypeAliasCount : integer;
begin
  Result := FTypeAliases.Count;
end;

function TSepiMetaOmniContainer.GetTypeAliases(Index : integer) : TSepiTypeAlias;
begin
  Result := TSepiTypeAlias(FTypeAliases.Objects[Index]);
end;

procedure TSepiMetaOmniContainer.ChildAdded(Child : TSepiMeta);
begin
  inherited;
  if Child is TSepiType then
    FTypes.AddMeta(Child);
  if Child is TSepiConstant then
    FConstants.AddMeta(Child);
  if Child is TSepiTypeAlias then
    FTypeAliases.AddMeta(Child);
end;

procedure TSepiMetaOmniContainer.ChildRemoving(Child : TSepiMeta);
begin
  if Child is TSepiTypeAlias then
    FTypeAliases.Remove(Child);
  if Child is TSepiConstant then
    FConstants.Remove(Child);
  if Child is TSepiType then
    FTypes.Remove(Child);
  inherited;
end;

////////////////////////////
/// Classe TSepiMetaRoot ///
////////////////////////////

constructor TSepiMetaRoot.Create;
begin
  inherited Create(nil, '');
  FRoot := Self;
end;

function TSepiMetaRoot.GetUnitCount : integer;
begin
  Result := FChildren.Count;
end;

function TSepiMetaRoot.GetUnits(Index : integer) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit(FChildren.Objects[Index]);
end;

procedure TSepiMetaRoot.LoadUnit(UnitName : string);
begin
  { TODO 2 -cMetaunit�s : Charger une unit� par son nom (ce peut �tre une unit� "syst�me" ou non) }
end;

////////////////////////////
/// Classe TSepiMetaUnit ///
////////////////////////////

constructor TSepiMetaUnit.Load(AOwner : TSepiMeta; Stream : TStream);
var Str : string;
begin
  inherited;
  FOwningUnit := Self;
  FCurrentVisibility := mvPublic;
  FLoadingReferences := TStringList.Create;
  FLoadingReferences.Add(''); // 0 is nil
  while True do
  begin
    Str := ReadStrFromStream(Stream);
    if Str = '' then break;
    FLoadingReferences.Add(Str);
  end;
end;

constructor TSepiMetaUnit.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FOwningUnit := Self;
  FCurrentVisibility := mvPublic;
  FLoadingReferences := nil;
end;

procedure TSepiMetaUnit.SetCurrentVisibility(Value : TSepiMemberVisibility);
begin
  if Value in [mvPublic, mvPublished] then
    FCurrentVisibility := mvPublic
  else
    FCurrentVisibility := mvPrivate;
end;

procedure TSepiMetaUnit.Loaded;
var I : integer;
begin
  for I := 1 to FLoadingReferences.Count-1 do // 0 is nil
    FLoadingReferences.Objects[I] := Root.FindMeta(FLoadingReferences[I]);
  inherited;
  FLoadingReferences.Free;
  FLoadingReferences := nil;
end;

procedure TSepiMetaUnit.LoadRef(var Ref);
begin
  { This method may only be called from the Loaded method of the items
    contained in this unit. }
  TObject(Ref) := FLoadingReferences.Objects[integer(Ref)];
end;

/////////////////////////////
/// Classe TSepiTypeAlias ///
/////////////////////////////

constructor TSepiTypeAlias.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FDest, 4);
end;

constructor TSepiTypeAlias.Create(AOwner : TSepiMeta; AName : string;
  ADest : TSepiType);
begin
  inherited Create(AOwner, AName);
  FDest := ADest;
end;

procedure TSepiTypeAlias.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FDest);
end;

////////////////////////////
/// Classe TSepiConstant ///
////////////////////////////

constructor TSepiConstant.Load(AOwner : TSepiMeta; Stream : TStream);
var ValueLength : integer;
begin
  inherited;
  Stream.ReadBuffer(FType, 4);
  FValue := TMemoryStream.Create;

  Stream.ReadBuffer(ValueLength, 4);
  if ValueLength > 0 then // to prevent a CopyFrom(0) *
    FValue.CopyFrom(Stream, ValueLength);

  { * I reckon a null ValueLength shouldn't happen. But as a newly created
    TSepiConstant has got a null Value, it is more logical and safer to allow
    and check for a null ValueLength. }
end;

constructor TSepiConstant.Create(AOwner : TSepiMeta; AName : string;
  AType : TSepiType);
begin
  inherited Create(AOwner, AName);
  FType := AType;
  FValue := TMemoryStream.Create;
end;

procedure TSepiConstant.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FType);
end;

////////////////////////////////
/// Classe TSepiMetaVariable ///
////////////////////////////////

constructor TSepiMetaVariable.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FType, 4);
end;

constructor TSepiMetaVariable.Create(AOwner : TSepiMeta; AName : string;
  AType : TSepiType);
begin
  inherited Create(AOwner, AName);
  FType := AType;
end;

procedure TSepiMetaVariable.Loaded;
begin
  OwningUnit.LoadRef(FType);
end;

/////////////////////////////
/// Classe TSepiMetaParam ///
/////////////////////////////

constructor TSepiMetaParam.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FParamKind, 1);
end;

constructor TSepiMetaParam.Create(AOwner : TSepiMeta; AName : string;
  AType : TSepiType; AParamKind : TSepiParamKind = pkInParam);
begin
  inherited Create(AOwner, AName, AType);
  FParamKind := AParamKind;
end;

function TSepiMetaParam.CompatibleWith(AVariable : TSepiMetaVariable) : boolean;
begin
  if (AVariable is TSepiMetaParam) and
     (TSepiMetaParam(AVariable).FParamKind <> FParamKind) then
    Result := False
  else if FParamKind in [pkInParam, pkConstant] then
    Result := FType.CompatibleWith(AVariable.FType)
  else
    Result := FType = AVariable.FType;
end;

///////////////////////////////////
/// Classe TSepiMethodSignature ///
///////////////////////////////////

constructor TSepiMethodSignature.Create(AOwner : TSepiMeta);
begin
  inherited Create;
  FOwner := AOwner;
  FParams := TSepiMetaList.Create;
  FReturnType := nil;
end;

destructor TSepiMethodSignature.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TSepiMethodSignature.Load(Stream : TStream);
var ParamCount : integer;
begin
  Stream.ReadBuffer(ParamCount, 4);
  while ParamCount > 0 do
  begin
    FParams.AddMeta(TSepiMetaParam.Load(FOwner, Stream));
    dec(ParamCount);
  end;
  Stream.ReadBuffer(FReturnType, 4);
end;

procedure TSepiMethodSignature.Loaded;
begin
  inherited;
  FOwner.OwningUnit.LoadRef(FReturnType);
end;

function TSepiMethodSignature.GetParamCount : integer;
begin
  Result := FParams.Count;
end;

function TSepiMethodSignature.GetParams(Index : integer) : TSepiMetaParam;
begin
  Result := TSepiMetaParam(FParams.Objects[Index]);
end;

function TSepiMethodSignature.CompatibleWith(ASignature : TSepiMethodSignature) : boolean;
var I : integer;
begin
  Result := False;

  if FParams.Count <> ASignature.FParams.Count then exit;
  for I := 0 to FParams.Count-1 do
    if not Params[I].CompatibleWith(ASignature.Params[I]) then exit;

  if FReturnType <> ASignature.FReturnType then exit;

  Result := True;
end;

//////////////////////////////
/// Classe TSepiMetaMethod ///
//////////////////////////////

constructor TSepiMetaMethod.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FSignature := TSepiMethodSignature.Create(Self);
  FSignature.Load(Stream);
  Stream.ReadBuffer(FKind, 1);
  Stream.ReadBuffer(FOverrideState, 1);
  Stream.ReadBuffer(FAbstract, 1);
  FInherited := nil;
end;

constructor TSepiMetaMethod.Create(AOwner : TSepiMeta; AName : string;
  AKind : TSepiMethodKind = mkNormal;
  AOverrideState : TSepiMethodOverrideState = osNone;
  AAbstract : boolean = False);
begin
  inherited Create(AOwner, AName);
  FSignature := TSepiMethodSignature.Create(Self);
  FKind := AKind;
  FOverrideState := AOverrideState;
  FAbstract := AAbstract and (FOverrideState in [osVirtual, osDynamic]);
  FindInherited;
end;

destructor TSepiMetaMethod.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

procedure TSepiMetaMethod.FindInherited;
var Overloaded : boolean;
    LookFor : string;
begin
  LookFor := FName;

  Overloaded := Copy(FName, 1, 3) = 'OL$';
  if Overloaded then
  begin
    Delete(LookFor, 1, 3);
    Delete(LookFor, Pos('$', LookFor), Length(LookFor));
  end;

  FInherited := nil;
  { TODO 3 -cMetaunit�s : Trouver la m�thode h�rit�e }
end;

procedure TSepiMetaMethod.Loaded;
begin
  inherited;
  FSignature.Loaded;
  FindInherited;
end;

//////////////////////////////////
/// Classe TSepiMetaSepiMethod ///
//////////////////////////////////

constructor TSepiMetaSepiMethod.Load(AOwner : TSepiMeta; Stream : TStream);
var Count : integer;
begin
  inherited;
  FVariables := TSepiMetaList.Create;
  FCode := TMemoryStream.Create;

  Stream.ReadBuffer(Count, 4);
  while Count > 0 do
  begin
    FVariables.AddMeta(TSepiMetaVariable.Load(Self, Stream));
    dec(Count);
  end;

  Stream.ReadBuffer(Count, 4);
  if Count > 0 then // To prevent a CopyFrom(0)
    FCode.CopyFrom(Stream, Count);
end;

constructor TSepiMetaSepiMethod.Create(AOwner : TSepiMeta; AName : string;
  AKind : TSepiMethodKind = mkNormal;
  AOverrideState : TSepiMethodOverrideState = osNone;
  AAbstract : boolean = False);
begin
  inherited Create(AOwner, AName, AKind, AOverrideState, AAbstract);
  FCode := TMemoryStream.Create;
end;

destructor TSepiMetaSepiMethod.Destroy;
begin
  FCode.Free;
  inherited Destroy;
end;

function TSepiMetaSepiMethod.GetVariableCount : integer;
begin
  Result := FVariables.Count;
end;

function TSepiMetaSepiMethod.GetVariables(Index : integer) : TSepiMetaVariable;
begin
  Result := TSepiMetaVariable(FVariables.Objects[Index]);
end;

////////////////////////////////////
/// Classe TSepiMetaDelphiMethod ///
////////////////////////////////////

constructor TSepiMetaDelphiMethod.Create(AOwner : TSepiMeta; AName : string;
  ACode : Pointer; AKind : TSepiMethodKind = mkNormal;
  AOverrideState : TSepiMethodOverrideState = osNone;
  AAbstract : boolean = False;
  ACallConvention : TSepiCallConvention = ccRegister);
begin
  inherited Create(AOwner, AName, AKind, AOverrideState, AAbstract);
  FCallConvention := ACallConvention;
  FCode := ACode;
end;

////////////////////////////////////////
/// Classe TSepiMetaOverloadedMethod ///
////////////////////////////////////////

constructor TSepiMetaOverloadedMethod.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FMethodCount, 4);
end;

constructor TSepiMetaOverloadedMethod.Create(AOwner : TSepiMeta; AName : string);
begin
  inherited Create(AOwner, AName);
  FMethodCount := 0;
end;

function TSepiMetaOverloadedMethod.NextID : integer;
begin
  Result := FMethodCount;
  inc(FMethodCount);
end;

function TSepiMetaOverloadedMethod.FindMethod(Signature : TSepiMethodSignature) : TSepiMetaMethod;
var I : integer;
begin
  for I := 0 to FMethodCount-1 do
  begin
    Result := TSepiMetaMethod(FOwner.FindMeta(Format('OL$%s$%d', [FName, I])));
    if Result.FSignature.CompatibleWith(Signature) then exit;
  end;
  Result := nil;
end;

////////////////////////////////
/// Classe TSepiMetaProperty ///
////////////////////////////////

constructor TSepiMetaProperty.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FSignature := TSepiMethodSignature.Create(Self);
  FSignature.Load(Stream);

  Stream.ReadBuffer(FReadAccess, 4);
  FReadAccessKind := pakNone;
  FReadVariableAccess := nil;
  FReadMethodAccess := nil;

  Stream.ReadBuffer(FWriteAccess, 4);
  FWriteAccessKind := pakNone;
  FWriteVariableAccess := nil;
  FWriteMethodAccess := nil;
end;

constructor TSepiMetaProperty.Create(AOwner : TSepiMeta; AName : string;
  AReadAccess : TSepiMeta; AWriteAccess : TSepiMeta);
begin
  inherited Create(AOwner, AName);
  FSignature := TSepiMethodSignature.Create(Self);

  FReadAccess := AReadAccess;
  FReadAccessKind := pakNone;
  FReadVariableAccess := nil;
  FReadMethodAccess := nil;

  FWriteAccess := AWriteAccess;
  FWriteAccessKind := pakNone;
  FWriteVariableAccess := nil;
  FWriteMethodAccess := nil;

  DevelopAccess;
end;

destructor TSepiMetaProperty.Destroy;
begin
  FSignature.Free;
  inherited;
end;

procedure TSepiMetaProperty.DevelopAccess;
begin
  if Assigned(FReadAccess) then
  begin
    if FReadAccess is TSepiMetaVariable then
    begin
      FReadAccessKind := pakVariable;
      FReadVariableAccess := TSepiMetaVariable(FReadAccess);
    end else
    if FReadAccess is TSepiMetaMethod then
    begin
      FReadAccessKind := pakMethod;
      FReadMethodAccess := TSepiMetaMethod(FReadAccess);
    end else
    FReadAccess := nil;
  end;

  if Assigned(FWriteAccess) then
  begin
    if FWriteAccess is TSepiMetaVariable then
    begin
      FWriteAccessKind := pakVariable;
      FWriteVariableAccess := TSepiMetaVariable(FWriteAccess);
    end else
    if FWriteAccess is TSepiMetaMethod then
    begin
      FWriteAccessKind := pakMethod;
      FReadMethodAccess := TSepiMetaMethod(FWriteAccess);
    end else
    FWriteAccess := nil;
  end;
end;

procedure TSepiMetaProperty.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FReadAccess);
  OwningUnit.LoadRef(FWriteAccess);
  DevelopAccess;
end;

initialization
  SepiImportedUnits := TStringList.Create;
  with TStringList(SepiImportedUnits) do
  begin
    CaseSensitive := True;
    Duplicates := dupIgnore;
  end;
finalization
  SepiImportedUnits.Free;
end.

