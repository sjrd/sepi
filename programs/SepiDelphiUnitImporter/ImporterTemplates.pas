unit ImporterTemplates;

interface

uses
  SysUtils, Classes, StrUtils, TypInfo, ScUtils, ScDelphiLanguage;

type
  {*
    Simple template processor
    @author sjrd
    @version 1.0
  *}
  TTemplate = class
  private
    Contents: string;             /// Contents of the template file
    ParamNames: TStrings;         /// Names of the parameters
    ParamValues: array of string; /// Values of the parameters
  public
    constructor Create(const FileName: TFileName);
    destructor Destroy; override;

    procedure Clear;

    procedure SetParam(const ParamName, ParamValue: string); overload;
    procedure SetParam(const ParamName: string;
      ParamValue: TStrings); overload;
    procedure SetParam(const ParamName: string;
      ParamValue: Integer); overload;
    procedure SetParam(const ParamName: string;
      ParamValue: Boolean); overload;
    procedure SetParam(const ParamName: string;
      const ParamValue; TypeInfo: PTypeInfo); overload;

    procedure AddToParam(const ParamName, ParamValue: string); overload;
    procedure AddToParam(const ParamName: string;
      ParamValue: TStrings); overload;

    function Process: string;
  end;

implementation

{*
  Creates a new template
  @param FileName   Name of the template file
*}
constructor TTemplate.Create(const FileName: TFileName);
begin
  inherited Create;

  with TStringList.Create do
  try
    LoadFromFile(FileName);
    Contents := Text;
  finally
    Free;
  end;

  ParamNames := TStringList.Create;
  TStringList(ParamNames).CaseSensitive := True;
end;

{*
  [@inheritDoc]
*}
destructor TTemplate.Destroy;
begin
  ParamNames.Free;
  inherited;
end;

{*
  Clears all parameters
*}
procedure TTemplate.Clear;
var
  I: Integer;
begin
  ParamNames.Clear;
  for I := Length(ParamValues)-1 downto 0 do
    ParamValues[I] := '';
end;

{*
  Sets up a string parameter
  @param ParamName    Parameter name
  @param ParamValue   Parameter value
*}
procedure TTemplate.SetParam(const ParamName, ParamValue: string);
const
  AllocBy = $10;
var
  Index: Integer;
begin
  Index := ParamNames.IndexOf(ParamName);
  if Index < 0 then
  begin
    Index := ParamNames.Add(ParamName);
    if Index >= Length(ParamValues) then
      SetLength(ParamValues, Index+AllocBy);
  end;
  ParamValues[Index] := ParamValue;
end;

{*
  Sets up a TStrings parameter
  @param ParamName    Parameter name
  @param ParamValue   Parameter value
*}
procedure TTemplate.SetParam(const ParamName: string; ParamValue: TStrings);
begin
  SetParam(ParamName, ParamValue.Text);
end;

{*
  Sets up an integer parameter
  @param ParamName    Parameter name
  @param ParamValue   Parameter value
*}
procedure TTemplate.SetParam(const ParamName: string; ParamValue: Integer);
begin
  SetParam(ParamName, IntToStr(ParamValue));
end;

{*
  Sets up a boolean parameter
  @param ParamName    Parameter name
  @param ParamValue   Parameter value
*}
procedure TTemplate.SetParam(const ParamName: string; ParamValue: Boolean);
begin
  SetParam(ParamName, BooleanIdents[ParamValue]);
end;

{*
  Sets up an enumerated or set parameter
  @param ParamName    Parameter name
  @param ParamValue   Parameter value
*}
procedure TTemplate.SetParam(const ParamName: string; const ParamValue;
  TypeInfo: PTypeInfo);
var
  StrValue: string;
begin
  if TypeInfo.Kind = tkSet then
    StrValue := EnumSetToStr(ParamValue, TypeInfo)
  else
    StrValue := GetEnumName(TypeInfo, Byte(ParamValue));
  SetParam(ParamName, StrValue);
end;

{*
  Adds a string to a parameter
  @param ParamName    Parameter name
  @param ParamValue   Parameter value
*}
procedure TTemplate.AddToParam(const ParamName, ParamValue: string);
var
  Index: Integer;
begin
  Index := ParamNames.IndexOf(ParamName);
  if Index < 0 then
    SetParam(ParamName, ParamValue)
  else
    ParamValues[Index] := ParamValues[Index] + ParamValue;
end;

{*
  Adds a TStrings to a parameter
  @param ParamName    Parameter name
  @param ParamValue   Parameter value
*}
procedure TTemplate.AddToParam(const ParamName: string;
  ParamValue: TStrings);
begin
  AddToParam(ParamName, ParamValue.Text);
end;

{*
  Processes the template
  @return The template processed
*}
function TTemplate.Process: string;
var
  I: Integer;
  OldResult: string;
begin
  Result := Contents;

  repeat
    OldResult := Result;
    for I := 0 to ParamNames.Count-1 do
      Result := AnsiReplaceStr(Result, '(#'+ParamNames[I]+'#)',
        ParamValues[I]);
  until Result = OldResult;
end;

end.

