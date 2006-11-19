{*
  Définit les classes de gestion des valeurs à l'exécution
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiValues;

interface

uses
  Classes, SysUtils, Contnrs, SepiMetaUnits, SepiTypes, ScUtils;
(*
type
  TSepiValue = class(TPersistent)
  private
    FType : TSepiType;
  public
    constructor Create(AType : TSepiType); virtual;

    procedure Load(Stream : TStream); virtual; abstract;

    class function CreateFromType(AType : TSepiType) : TSepiValue;

    property ValueType : TSepiType read FType;
  end;

  TSepiIntegerValue = class(TSepiValue)
  private
    FType : TSepiIntegerType;
    FValue : integer;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property IntType : TSepiIntegerType read FType;
    property Value : integer read FValue write FValue;
  end;

  TSepiFloatValue = class(TSepiValue)
  private
    FType : TSepiFloatType;
    FValue : Double;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property FloatType : TSepiFloatType read FType;
    property Value : Double read FValue write FValue;
  end;

  TSepiStringValue = class(TSepiValue)
  private
    FType : TSepiStringType;
    FValue : string;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property StrType : TSepiStringType read FType;
    property Value : string read FValue write FValue;
  end;

  TSepiBooleanValue = class(TSepiValue)
  private
    FType : TSepiBooleanType;
    FValue : boolean;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property BoolType : TSepiBooleanType read FType;
    property Value : boolean read FValue write FValue;
  end;

  TSepiEnumValue = class(TSepiValue)
  private
    FType : TSepiEnumType;
    FValue : integer;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property EnumType : TSepiEnumType read FType;
    property Value : integer read FValue write FValue;
  end;

  TSepiSetValue = class(TSepiValue)
  private
    FType : TSepiSetType;
    FValue : TSysByteSet;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property SetType : TSepiSetType read FType;
    property Value : TSysByteSet read FValue write FValue;
  end;

  TSepiRecordValue = class(TSepiValue)
  private
    FType : TSepiRecordType;
    FValues : array of TSepiValue;

    function GetValues(VarName : string) : TSepiValue;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property RecordType : TSepiRecordType read FType;
    property Values[VarName : string] : TSepiValue read GetValues;
  end;

  TSepiClassValue = class(TSepiValue)
  private
    FType : TSepiClassType;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property ClassType : TSepiClassType read FType;
  end;

  TSepiMetaClassValue = class(TSepiValue)
  private
    FType : TSepiMetaClassType;
  public
    constructor Create(AType : TSepiType); override;

    procedure Load(Stream : TStream); override;

    property MetaClassType : TSepiMetaClassType read FType;
  end;

  TSepiArrayValue = class(TSepiValue)
  private
    FType : TSepiArrayType;
    FValues : array of TSepiValue;
  public
    constructor Create(AType : TSepiType); override;
    destructor Destroy; override;

    procedure Load(Stream : TStream); override;

    function GetValues(Indices : array of integer) : TSepiValue;

    property ArrayType : TSepiArrayType read FType;
  end;

  TSepiDynArrayValue = class(TSepiValue)
  private
    FType : TSepiDynArrayType;
    FValues : TObjectList;

    function GetCount : integer;
    procedure SetCount(Value : integer);
    function GetValues(Index : integer) : TSepiValue;
  public
    constructor Create(AType : TSepiType); override;
    destructor Destroy; override;

    procedure Load(Stream : TStream); override;

    function Add : TSepiValue;
    function Insert(Index : integer) : TSepiValue;
    procedure Delete(Index : integer);

    property DynArrayType : TSepiDynArrayType read FType;
    property Count : integer read GetCount write SetCount;
    property Values[index : integer] : TSepiValue read GetValues;
  end;

  TSepiDelegateItem = class
  public
    function Equals(DelegateItem : TSepiDelegateItem) : boolean; virtual; abstract;
    procedure CallProc(Params : TObjectList); virtual; abstract;
    function CallFunc(Params : TObjectList) : TSepiValue; virtual; abstract;
  end;

  TSepiDelegateToSepiMethod = class(TSepiDelegateItem)
  public
    function Equals(DelegateItem : TSepiDelegateItem) : boolean; override;
    procedure CallProc(Params : TObjectList); override;
    function CallFunc(Params : TObjectList) : TSepiValue; override;
  end;

  TSepiDelegateToDelphiRoutine = class(TSepiDelegateItem)
  private
    FRoutine : Pointer;
  public
    constructor Create(ARoutine : Pointer);

    function Equals(DelegateItem : TSepiDelegateItem) : boolean; override;
    procedure CallProc(Params : TObjectList); override;
    function CallFunc(Params : TObjectList) : TSepiValue; override;
  end;

  TSepiDelegateToDelphiMethod = class(TSepiDelegateToDelphiRoutine)
  private
    FInstance : TSepiIntegerValue;
  public
    constructor Create(AMethod : TMethod);
    destructor Destroy; override;

    function Equals(DelegateItem : TSepiDelegateItem) : boolean; override;
    procedure CallProc(Params : TObjectList); override;
    function CallFunc(Params : TObjectList) : TSepiValue; override;
  end;

  TSepiDelegateValue = class(TSepiValue)
  private
    FType : TSepiDelegateType;
    FValues : TObjectList;
  public
    constructor Create(AType : TSepiType); override;
    destructor Destroy; override;

    procedure Include(DelegateItem : TSepiDelegateItem);
    procedure Exclude(DelegateItem : TSepiDelegateItem);

    procedure CallProc(Params : TObjectList);
    function CallFunc(Params : TObjectList) : TSepiValue;
  end;
*)
implementation
(*
{-------------------}
{ Classe TSepiValue }
{-------------------}

constructor TSepiValue.Create(AType : TSepiType);
begin
  inherited Create;
  FType := AType;
end;

class function TSepiValue.CreateFromType(AType : TSepiType) : TSepiValue;
begin
  Result := nil;
end;

{--------------------------}
{ Classe TSepiIntegerValue }
{--------------------------}

constructor TSepiIntegerValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiIntegerType(AType);
end;

procedure TSepiIntegerValue.Load(Stream : TStream);
begin
end;

{------------------------}
{ Classe TSepiFloatValue }
{------------------------}

constructor TSepiFloatValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiFloatType(AType);
end;

procedure TSepiFloatValue.Load(Stream : TStream);
begin
end;

{-------------------------}
{ Classe TSepiStringValue }
{-------------------------}

constructor TSepiStringValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiStringType(AType);
end;

procedure TSepiStringValue.Load(Stream : TStream);
begin
end;

{--------------------------}
{ Classe TSepiBooleanValue }
{--------------------------}

constructor TSepiBooleanValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiBooleanType(AType);
end;

procedure TSepiBooleanValue.Load(Stream : TStream);
begin
end;

{-----------------------}
{ Classe TSepiEnumValue }
{-----------------------}

constructor TSepiEnumValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiEnumType(AType);
end;

procedure TSepiEnumValue.Load(Stream : TStream);
begin
end;

{----------------------}
{ Classe TSepiSetValue }
{----------------------}

constructor TSepiSetValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiSetType(AType);
end;

procedure TSepiSetValue.Load(Stream : TStream);
begin
end;

{-------------------------}
{ Classe TSepiRecordValue }
{-------------------------}

constructor TSepiRecordValue.Create(AType : TSepiType);
var I : integer;
begin
  inherited;
  FType := TSepiRecordType(AType);

//  SetLength(FValues, FType.ObjectCount);
  for I := 0 to Length(FValues)-1 do
    FValues[I] := TSepiValue.CreateFromType(FType.Variables[I].VarType);
end;

function TSepiRecordValue.GetValues(VarName : string) : TSepiValue;
var I : integer;
begin
  for I := 0 to Length(FValues)-1 do
  begin
    if FType.Variables[I].Name = VarName then
    begin
      Result := FValues[I];
      exit;
    end;
  end;
  Result := nil;
end;

procedure TSepiRecordValue.Load(Stream : TStream);
var I : integer;
begin
  for I := 0 to Length(FValues)-1 do
    FValues[I].Load(Stream);
end;

{------------------------}
{ Classe TSepiClassValue }
{------------------------}

constructor TSepiClassValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiClassType(AType);
end;

procedure TSepiClassValue.Load(Stream : TStream);
begin
end;

{----------------------------}
{ Classe TSepiMetaClassValue }
{----------------------------}

constructor TSepiMetaClassValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiMetaClassType(AType);
end;

procedure TSepiMetaClassValue.Load(Stream : TStream);
begin
end;

{------------------------}
{ Classe TSepiArrayValue }
{------------------------}

constructor TSepiArrayValue.Create(AType : TSepiType);
var Size, I : integer;
begin
  inherited;
  FType := TSepiArrayType(AType);

  Size := 1;
  for I := 0 to FType.DimCount-1 do
    Size := Size * FType.Dimensions[I];

  SetLength(FValues, Size);
  for I := 0 to Length(FValues)-1 do
    FValues[I] := TSepiValue.CreateFromType(FType.ItemType);
end;

destructor TSepiArrayValue.Destroy;
var I : integer;
begin
  if Assigned(FValues[0]) then for I := 0 to Length(FValues)-1 do
    FValues[I].Free;
  SetLength(FValues, 0);
  inherited;
end;

function TSepiArrayValue.GetValues(Indices : array of integer) : TSepiValue;
var Index, I : integer;
begin
  if Length(Indices) <> FType.DimCount then
    raise Exception.Create('');

  Index := 0;
  for I := 0 to FType.DimCount do
  begin
    if (Indices[I] < 0) or (Indices[I] >= FType.Dimensions[I]) then
      raise Exception.Create('');

    Index := Index*FType.Dimensions[I];
    inc(Index, Indices[I]);
  end;

  Result := FValues[Index];
end;

procedure TSepiArrayValue.Load(Stream : TStream);
var I : integer;
begin
  for I := 0 to Length(FValues)-1 do
    FValues[I].Load(Stream);
end;

{---------------------------}
{ Classe TSepiDynArrayValue }
{---------------------------}

constructor TSepiDynArrayValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiDynArrayType(AType);
  FValues := TObjectList.Create;
end;

destructor TSepiDynArrayValue.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TSepiDynArrayValue.GetCount : integer;
begin
  Result := FValues.Count;
end;

procedure TSepiDynArrayValue.SetCount(Value : integer);
var I : integer;
begin
  FValues.Count := Value;

  for I := Value-1 downto 0 do
  begin
    if Assigned(FValues[I]) then Break;
    FValues[I] := TSepiValue.CreateFromType(FType.ItemType);
  end;
end;

function TSepiDynArrayValue.GetValues(Index : integer) : TSepiValue;
begin
  Result := TSepiValue(FValues[Index]);
end;

procedure TSepiDynArrayValue.Load(Stream : TStream);
var I : integer;
begin
  Stream.ReadBuffer(I, 4);
  Count := I;

  for I := 0 to FValues.Count-1 do
    TSepiValue(FValues[I]).Load(Stream);
end;

function TSepiDynArrayValue.Add : TSepiValue;
begin
  Result := TSepiValue.CreateFromType(FType.ItemType);
  FValues.Add(Result);
end;

function TSepiDynArrayValue.Insert(Index : integer) : TSepiValue;
begin
  Result := TSepiValue.CreateFromType(FType.ItemType);
  FValues.Insert(Index, Result);
end;

procedure TSepiDynArrayValue.Delete(Index : integer);
begin
  FValues.Delete(Index);
end;

{----------------------------------}
{ Classe TSepiDelegateToSepiMethod }
{----------------------------------}

function TSepiDelegateToSepiMethod.Equals(DelegateItem : TSepiDelegateItem) : boolean;
begin
  Result := DelegateItem is TSepiDelegateToSepiMethod;
end;

procedure TSepiDelegateToSepiMethod.CallProc(Params : TObjectList);
begin
end;

function TSepiDelegateToSepiMethod.CallFunc(Params : TObjectList) : TSepiValue;
begin
  Result := nil;
end;

{-------------------------------------}
{ Classe TSepiDelegateToDelphiRoutine }
{-------------------------------------}

constructor TSepiDelegateToDelphiRoutine.Create(ARoutine : Pointer);
begin
  inherited Create;
  FRoutine := ARoutine;
end;

function TSepiDelegateToDelphiRoutine.Equals(DelegateItem : TSepiDelegateItem) : boolean;
begin
  Result := (DelegateItem is TSepiDelegateToDelphiRoutine) and
    (TSepiDelegateToDelphiRoutine(DelegateItem).FRoutine = FRoutine);
end;

procedure TSepiDelegateToDelphiRoutine.CallProc(Params : TObjectList);
begin
end;

function TSepiDelegateToDelphiRoutine.CallFunc(Params : TObjectList) : TSepiValue;
begin
  Result := nil;
end;

{------------------------------------}
{ Classe TSepiDelegateToDelphiMethod }
{------------------------------------}

constructor TSepiDelegateToDelphiMethod.Create(AMethod : TMethod);
type
  TPointerToInt = record
    case integer of
      0 : (PointerValue : Pointer);
      1 : (IntValue : integer);
  end;
var PointerToInt : TPointerToInt;
begin
  inherited Create(AMethod.Code);
  PointerToInt.PointerValue := AMethod.Data;
  FInstance := TSepiIntegerValue(TSepiValue.CreateFromType(TSepiIntegerType.Create(nil, '')));
  FInstance.Value := PointerToInt.IntValue;
end;

destructor TSepiDelegateToDelphiMethod.Destroy;
var InstanceType : TSepiType;
begin
  InstanceType := FInstance.ValueType;
  FInstance.Free;
  InstanceType.Free;
  inherited Destroy;
end;

function TSepiDelegateToDelphiMethod.Equals(DelegateItem : TSepiDelegateItem) : boolean;
begin
  Result := (DelegateItem is TSepiDelegateToDelphiMethod) and
    (TSepiDelegateToDelphiMethod(DelegateItem).FRoutine = FRoutine) and
    (TSepiDelegateToDelphiMethod(DelegateItem).FInstance = FInstance);
end;

procedure TSepiDelegateToDelphiMethod.CallProc(Params : TObjectList);
begin
  Params.Insert(0, FInstance);
  inherited CallProc(Params);
  Params.Extract(FInstance);
end;

function TSepiDelegateToDelphiMethod.CallFunc(Params : TObjectList) : TSepiValue;
begin
  Params.Insert(0, FInstance);
  Result := inherited CallFunc(Params);
  Params.Extract(FInstance);
end;

{---------------------------}
{ Classe TSepiDelegateValue }
{---------------------------}

constructor TSepiDelegateValue.Create(AType : TSepiType);
begin
  inherited;
  FType := TSepiDelegateType(AType);
  FValues := TObjectList.Create(False);
end;

destructor TSepiDelegateValue.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TSepiDelegateValue.Include(DelegateItem : TSepiDelegateItem);
var I : integer;
begin
  for I := 0 to FValues.Count-1 do
    if TSepiDelegateItem(FValues[I]).Equals(DelegateItem) then exit;
  FValues.Add(DelegateItem);
end;

procedure TSepiDelegateValue.Exclude(DelegateItem : TSepiDelegateItem);
var I : integer;
begin
  for I := 0 to FValues.Count-1 do
  begin
    if TSepiDelegateItem(FValues[I]).Equals(DelegateItem) then
    begin
      FValues.Delete(I);
      exit;
    end;
  end;
end;

procedure TSepiDelegateValue.CallProc(Params : TObjectList);
var I : integer;
begin
  for I := 0 to FValues.Count-1 do
    TSepiDelegateItem(FValues[I]).CallProc(Params);
end;

function TSepiDelegateValue.CallFunc(Params : TObjectList) : TSepiValue;
begin
  if FValues.Count = 1 then
    Result := TSepiDelegateItem(FValues[0]).CallFunc(Params)
  else
    raise Exception.Create('');
end;
*)
end.

