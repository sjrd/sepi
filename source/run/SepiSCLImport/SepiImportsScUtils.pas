{*
  Importe l'unité ScUtils dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsScUtils;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ScUtils, Types;

implementation

{ You must not localize any of the strings this unit contains! }

{-----------------}
{ T3DPoint import }
{-----------------}

function SepiImportT3DPoint(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'T3DPoint', False, True);

  with Result do
  begin
    AddField('X', System.TypeInfo(integer));
    AddField('Y', System.TypeInfo(integer));
    AddField('Z', System.TypeInfo(integer));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function IIF_0(Cond : boolean; Int1, Int2 : integer) : integer;
begin
  Result := IIF(Cond, Int1, Int2);
end;

function IIF_1(Cond : boolean; Flo1, Flo2 : Double ) : Double;
begin
  Result := IIF(Cond, Flo1, Flo2);
end;

function IIF_2(Cond : boolean; Chr1, Chr2 : Char  ) : Char;
begin
  Result := IIF(Cond, Chr1, Chr2);
end;

function IIF_3(Cond : boolean; const Str1, Str2 : string) : string;
begin
  Result := IIF(Cond, Str1, Str2);
end;

function IIF_4(Cond : boolean; Obj1, Obj2 : TObject) : TObject;
begin
  Result := IIF(Cond, Obj1, Obj2);
end;

function IIF_5(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer;
begin
  Result := IIF(Cond, Ptr1, Ptr2);
end;

function IIF_6(Cond : boolean; Var1, Var2 : Variant) : Variant;
begin
  Result := IIF(Cond, Var1, Var2);
end;

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScUtils',
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
  TSepiMetaMethod.Create(Result, 'Dir', @Dir,
    'function: string');
  TSepiMetaOverloadedMethod.Create(Result, 'IIF');
  TSepiMetaMethod.Create(Result, 'OL$IIF$0', @IIF_0,
    'function(Cond : boolean; Int1, Int2 : integer) : integer');
  TSepiMetaMethod.Create(Result, 'OL$IIF$1', @IIF_1,
    'function(Cond : boolean; Flo1, Flo2 : Double ) : Double');
  TSepiMetaMethod.Create(Result, 'OL$IIF$2', @IIF_2,
    'function(Cond : boolean; Chr1, Chr2 : Char  ) : Char');
  TSepiMetaMethod.Create(Result, 'OL$IIF$3', @IIF_3,
    'function(Cond : boolean; const Str1, Str2 : string) : string');
  TSepiMetaMethod.Create(Result, 'OL$IIF$4', @IIF_4,
    'function(Cond : boolean; Obj1, Obj2 : TObject) : TObject');
  TSepiMetaMethod.Create(Result, 'OL$IIF$5', @IIF_5,
    'function(Cond : boolean; Ptr1, Ptr2 : Pointer) : Pointer');
  TSepiMetaMethod.Create(Result, 'OL$IIF$6', @IIF_6,
    'function(Cond : boolean; Var1, Var2 : Variant) : Variant');
  TSepiMetaMethod.Create(Result, 'MinMax', @MinMax,
    'function(Value, Min, Max : integer) : integer');
  TSepiMetaMethod.Create(Result, 'IntDiv', @IntDiv,
    'function(Op1, Op2 : integer) : integer');
  TSepiMetaMethod.Create(Result, 'IntMod', @IntMod,
    'function(Op1, Op2 : integer) : integer');
  TSepiMetaMethod.Create(Result, 'IntToBase', @IntToBase,
    'function(Value : integer; Base : Byte = 10) : string');
  TSepiMetaMethod.Create(Result, 'BaseToInt', @BaseToInt,
    'function(const Value : string; Base : Byte = 10) : integer');
  TSepiMetaMethod.Create(Result, 'BaseToIntDef', @BaseToIntDef,
    'function(const Value : string; Default : integer = 0; Base : Byte = 10 ) : integer');
  TSepiMetaMethod.Create(Result, 'ConvertDoubleToInt64', @ConvertDoubleToInt64,
    'function(Value : Double) : Int64');
  TSepiMetaMethod.Create(Result, 'ConvertInt64ToDouble', @ConvertInt64ToDouble,
    'function(Value : Int64) : Double');
  TSepiMetaMethod.Create(Result, 'WaitProcessMessages', @WaitProcessMessages,
    'procedure(Milliseconds : integer)');
  TSepiMetaMethod.Create(Result, 'IntToStr0', @IntToStr0,
    'function(Value, Digits : integer) : string');
  TSepiMetaMethod.Create(Result, 'ReadStrFromStream', @ReadStrFromStream,
    'function(Stream : TStream) : string');
  TSepiMetaMethod.Create(Result, 'WriteStrToStream', @WriteStrToStream,
    'procedure(Stream : TStream; const Str : string)');
  TSepiMetaMethod.Create(Result, 'CorrectFileName', @CorrectFileName,
    'function(const FileName : string; AcceptPathDelim : boolean = False ; AcceptDriveDelim : boolean = False ) : boolean');
  TSepiMetaMethod.Create(Result, 'IsNoPoint', @IsNoPoint,
    'function(const Point : TPoint) : boolean');
  TSepiMetaMethod.Create(Result, 'IsNo3DPoint', @IsNo3DPoint,
    'function(const Point3D : T3DPoint) : boolean');
  TSepiMetaMethod.Create(Result, 'IsNoGUID', @IsNoGUID,
    'function(const GUID : TGUID) : boolean');
  TSepiMetaMethod.Create(Result, 'Point3D', @Point3D,
    'function(X, Y, Z : integer) : T3DPoint');
  TSepiMetaMethod.Create(Result, 'Same3DPoint', @Same3DPoint,
    'function(const Point1, Point2 : T3DPoint) : boolean');
  TSepiMetaMethod.Create(Result, 'Point3DToString', @Point3DToString,
    'function(const Point3D : T3DPoint; const Delim : string = '' '' ) : string');
  TSepiMetaMethod.Create(Result, 'RunURL', @RunURL,
    'procedure(const URL : string; const Verb : string = ''open'')');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScUtils', ImportUnit);
end.

