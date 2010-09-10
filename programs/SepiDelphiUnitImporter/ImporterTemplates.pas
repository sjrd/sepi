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
  Processeur de template simple
  @author sjrd
  @version 1.0
*}
unit ImporterTemplates;

interface

uses
  SysUtils, Classes, StrUtils, TypInfo, ScUtils, ScTypInfo;

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

