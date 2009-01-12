{*
  Importe l'unité (#UnitName#) dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImports(#UnitName2#);

interface

uses
(#UsesList#)

var
  SepiImports(#UnitName2#)LazyLoad: Boolean = (#LazyLoad#);

implementation

{$R *.res}

const // don't localize
  UnitName = '(#UnitName2#)';
  ResourceName = 'SepiImports(#UnitName2#)';
  TypeCount = (#TypeCount#);
  MethodCount = (#MethodCount#);

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
(#TypeDecl#)(#ImportClassesDecls#)(#ImportClassesImpls#)
{---------------------}
{ Overloaded routines }
{---------------------}
(#Overloadeds#)
{-------------}
{ Unit import }
{-------------}

procedure GetMethodCode(Self, Sender: TObject; var Code: Pointer;
  var CodeHandler: TObject);
var
  Index: Integer;
begin
  Index := (Sender as TSepiMethod).Tag;
  if Index >= 0 then
    Code := MethodAddresses[Index];
end;

procedure GetTypeInfo(Self, Sender: TObject; var TypeInfo: PTypeInfo;
  var Found: Boolean);
var
  Index: Integer;
begin
  Index := (Sender as TSepiType).Tag;
  Found := Index >= -1;

  if Index >= 0 then
    TypeInfo := TypeInfoArray[Index];
end;

const
  GetMethodCodeEvent: TMethod = (Code: @GetMethodCode; Data: nil);
  GetTypeInfoEvent: TMethod = (Code: @GetTypeInfo; Data: nil);

function ImportUnit(Root: TSepiRoot): TSepiUnit;
var
  Stream: TStream;
begin
  Stream := TResourceStream.Create(SysInit.HInstance,
    ResourceName, RT_RCDATA);
  try
    Result := TSepiUnit.LoadFromStream(Root, Stream,
      SepiImports(#UnitName2#)LazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent));

    if SepiImports(#UnitName2#)LazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure InitTypeInfoArray;
begin
(#InitTypeInfoArray#)end;

procedure InitMethodAddresses;
begin
(#InitMethodAddresses#)end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;

  SepiRegisterImportedUnit('(#UnitName2#)', ImportUnit);
end.

