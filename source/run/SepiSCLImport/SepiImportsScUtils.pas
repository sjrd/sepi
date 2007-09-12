{*
  Importe l'unité ScUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScUtils;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScUtils, Types;

implementation

{ You must not localize any of the strings this unit contains! }

{-----------------}
{ T3DPoint import }
{-----------------}

function SepiImportT3DPoint(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'T3DPoint', False, True);

  with Result do
  begin
    AddField('X', System.TypeInfo(Integer));
    AddField('Y', System.TypeInfo(Integer));
    AddField('Z', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function IIF_0(Cond: Boolean; Int1, Int2: Integer): Integer;
begin
  Result := IIF(Cond, Int1, Int2);
end;

function IIF_1(Cond: Boolean; Flo1, Flo2: Double): Double;
begin
  Result := IIF(Cond, Flo1, Flo2);
end;

function IIF_2(Cond: Boolean; Chr1, Chr2: Char): Char;
begin
  Result := IIF(Cond, Chr1, Chr2);
end;

function IIF_3(Cond: Boolean; const Str1, Str2: string): string;
begin
  Result := IIF(Cond, Str1, Str2);
end;

function IIF_4(Cond: Boolean; Obj1, Obj2: TObject): TObject;
begin
  Result := IIF(Cond, Obj1, Obj2);
end;

function IIF_5(Cond: Boolean; Ptr1, Ptr2: Pointer): Pointer;
begin
  Result := IIF(Cond, Ptr1, Ptr2);
end;

function IIF_6(Cond: Boolean; Var1, Var2: Variant): Variant;
begin
  Result := IIF(Cond, Var1, Var2);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'ScUtils',
    ['Types', 'SysUtils', 'Classes']);

  // Types
  SepiImportT3DPoint(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSysByteSet));

  // Constants
  TSepiVariable.Create(Result, 'NoPoint',
    NoPoint, 'TPoint', True);
  TSepiVariable.Create(Result, 'No3DPoint',
    No3DPoint, 'T3DPoint', True);
  TSepiVariable.Create(Result, 'NoGUID',
    NoGUID, 'TGUID', True);

  // Routines
  TSepiMethod.Create(Result, 'Dir', @Dir,
    'function: string');
  TSepiOverloadedMethod.Create(Result, 'IIF');
  TSepiMethod.Create(Result, 'OL$IIF$0', @IIF_0,
    'function(Cond : boolean; Int1, Int2 : integer) : integer');
  TSepiMethod.Create(Result, 'OL$IIF$1', @IIF_1,
    'function(Cond : boolean; Flo1, Flo2 : Double ) : Double');
  TSepiMethod.Create(Result, 'OL$IIF$2', @IIF_2,
    'function(Cond : boolean; Chr1, Chr2 : Char  ) : Char');
  TSepiMethod.Create(Result, 'OL$IIF$3', @IIF_3,
    'function(Cond : boolean; const Str1, Str2 : string) : string');
  TSepiMethod.Create(Result, 'OL$IIF$4', @IIF_4,
    'function(Cond : boolean; Obj1, Obj2 : TObject) : TObject');
  TSepiMethod.Create(Result, 'OL$IIF$5', @IIF_5,
    'function(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer');
  TSepiMethod.Create(Result, 'OL$IIF$6', @IIF_6,
    'function(Cond : boolean; Var1, Var2 : Variant) : Variant');
  TSepiMethod.Create(Result, 'MinMax', @MinMax,
    'function(Value, Min, Max : integer) : integer');
  TSepiMethod.Create(Result, 'IntDiv', @IntDiv,
    'function(Op1, Op2 : integer) : integer');
  TSepiMethod.Create(Result, 'IntMod', @IntMod,
    'function(Op1, Op2 : integer) : integer');
  TSepiMethod.Create(Result, 'IntToBase', @IntToBase,
    'function(Value : integer; Base : Byte = 10) : string');
  TSepiMethod.Create(Result, 'BaseToInt', @BaseToInt,
    'function(const Value : string; Base : Byte = 10) : integer');
  TSepiMethod.Create(Result, 'BaseToIntDef', @BaseToIntDef,
    'function(const Value : string; Default : integer = 0; Base : Byte = 10 ) : integer');
  TSepiMethod.Create(Result, 'ConvertDoubleToInt64', @ConvertDoubleToInt64,
    'function(Value : Double) : Int64');
  TSepiMethod.Create(Result, 'ConvertInt64ToDouble', @ConvertInt64ToDouble,
    'function(Value : Int64) : Double');
  TSepiMethod.Create(Result, 'WaitProcessMessages', @WaitProcessMessages,
    'procedure(Milliseconds : integer)');
  TSepiMethod.Create(Result, 'IntToStr0', @IntToStr0,
    'function(Value, Digits : integer) : string');
  TSepiMethod.Create(Result, 'ReadStrFromStream', @ReadStrFromStream,
    'function(Stream : TStream) : string');
  TSepiMethod.Create(Result, 'WriteStrToStream', @WriteStrToStream,
    'procedure(Stream : TStream; const Str : string)');
  TSepiMethod.Create(Result, 'CorrectFileName', @CorrectFileName,
    'function(const FileName : string; AcceptPathDelim : boolean = False ; AcceptDriveDelim : boolean = False ) : boolean');
  TSepiMethod.Create(Result, 'SamePoint', @SamePoint,
    'function(const Point1, Point2 : TPoint) : boolean');
  TSepiMethod.Create(Result, 'Same3DPoint', @Same3DPoint,
    'function(const Point1, Point2 : T3DPoint) : boolean');
  TSepiMethod.Create(Result, 'IsNoPoint', @IsNoPoint,
    'function(const Point : TPoint) : boolean');
  TSepiMethod.Create(Result, 'IsNo3DPoint', @IsNo3DPoint,
    'function(const Point3D : T3DPoint) : boolean');
  TSepiMethod.Create(Result, 'SameGUID', @SameGUID,
    'function(const GUID1, GUID2 : TGUID) : boolean');
  TSepiMethod.Create(Result, 'IsNoGUID', @IsNoGUID,
    'function(const GUID : TGUID) : boolean');
  TSepiMethod.Create(Result, 'Point3D', @Point3D,
    'function(X, Y, Z : integer) : T3DPoint');
  TSepiMethod.Create(Result, 'Point3DToString', @Point3DToString,
    'function(const Point3D : T3DPoint; const Delim : string = '' '' ) : string');
  TSepiMethod.Create(Result, 'RunURL', @RunURL,
    'procedure(const URL : string; const Verb : string = ''open'')');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScUtils', ImportUnit);
end.

