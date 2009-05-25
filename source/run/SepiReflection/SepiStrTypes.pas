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
  Définit les classes de gestion des types chaîne
  @author sjrd
  @version 1.0
*}
unit SepiStrTypes;

interface

uses
  Classes, TypInfo, SepiReflectionCore;

type
  {*
    Type chaîne de caractères courte
    @author sjrd
    @version 1.0
  *}
  TSepiShortStringType = class(TSepiType)
  private
    FMaxLength: Integer; /// Longueur maximale
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetAlignment: Integer; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AMaxLength: Integer = 255);
    constructor Clone(AOwner: TSepiMeta; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;

    property MaxLength: Integer read FMaxLength;
  end;

  {*
    Type chaîne de caractères longue
    @author sjrd
    @version 1.0
  *}
  TSepiStringType = class(TSepiType)
  private
    FIsUnicode: Boolean; /// Indique si la chaîne est Unicode ou non
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AIsUnicode: Boolean = False);
    constructor Clone(AOwner: TSepiMeta; const AName: string;
      Source: TSepiType); override;

    function Equals(Other: TSepiType): Boolean; override;

    property IsUnicode: Boolean read FIsUnicode;
  end;

implementation

const
  // Tailles de structure TTypeData en fonction des types
  ShortStringTypeDataLength = SizeOf(Byte);
  StringTypeDataLength = 0;

{-----------------------------}
{ Classe TSepiShortStringType }
{-----------------------------}

{*
  Recense un type chaîne courte natif
*}
constructor TSepiShortStringType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type chaîne courte depuis un flux
*}
constructor TSepiShortStringType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  if not Native then
  begin
    AllocateTypeInfo(ShortStringTypeDataLength);
    Stream.ReadBuffer(TypeData^, ShortStringTypeDataLength);
  end else
    Stream.Seek(ShortStringTypeDataLength, soFromCurrent);

  ExtractTypeData;
end;

{*
  Crée un nouveau type chaîne courte
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AMaxLength   Longueur maximale
*}
constructor TSepiShortStringType.Create(AOwner: TSepiMeta;
  const AName: string; AMaxLength: Integer = 255);
begin
  inherited Create(AOwner, AName, tkString);

  AllocateTypeInfo(ShortStringTypeDataLength);
  TypeData.MaxLength := AMaxLength;

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
constructor TSepiShortStringType.Clone(AOwner: TSepiMeta; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiShortStringType).MaxLength);
end;

{*
  [@inheritDoc]
*}
procedure TSepiShortStringType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, ShortStringTypeDataLength);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiShortStringType.ExtractTypeData;
begin
  inherited;

  FMaxLength := TypeData.MaxLength;
  FSize := FMaxLength + 1;
  FParamBehavior.AlwaysByAddress := True;
  FResultBehavior := rbParameter;
end;

{*
  [@inheritDoc]
*}
function TSepiShortStringType.GetAlignment: Integer;
begin
  Result := 1;
end;

{*
  [@inheritDoc]
*}
function TSepiShortStringType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (MaxLength = TSepiShortStringType(Other).MaxLength);
end;

{------------------------}
{ Classe TSepiStringType }
{------------------------}

{*
  Recense un type chaîne longue natif
*}
constructor TSepiStringType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type chaîne longue depuis un flux
*}
constructor TSepiStringType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  if not Native then
  begin
    AllocateTypeInfo(StringTypeDataLength);
    Stream.ReadBuffer(TypeData^, StringTypeDataLength);
  end else
    Stream.Seek(StringTypeDataLength, soFromCurrent);

  ExtractTypeData;
end;

{*
  Crée un nouveau type chaîne longue
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AIsUnicode   Indique si la chaîne est Unicode ou non
*}
constructor TSepiStringType.Create(AOwner: TSepiMeta; const AName: string;
  AIsUnicode: Boolean = False);
var
  AKind: TTypeKind;
begin
  if AIsUnicode then
    AKind := tkWString
  else
    AKind := tkLString;
  inherited Create(AOwner, AName, AKind);

  AllocateTypeInfo(StringTypeDataLength);

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
constructor TSepiStringType.Clone(AOwner: TSepiMeta; const AName: string;
  Source: TSepiType);
begin
  Create(AOwner, AName, (Source as TSepiStringType).IsUnicode);
end;

{*
  [@inheritDoc]
*}
procedure TSepiStringType.Save(Stream: TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, StringTypeDataLength);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiStringType.ExtractTypeData;
begin
  inherited;

  FSize := 4;
  FNeedInit := True;
  FResultBehavior := rbParameter;
  FIsUnicode := Kind = tkWString;
end;

{*
  [@inheritDoc]
*}
function TSepiStringType.Equals(Other: TSepiType): Boolean;
begin
  Result := (ClassType = Other.ClassType) and
    (IsUnicode = TSepiStringType(Other).IsUnicode);
end;

initialization
  SepiRegisterMetaClasses([
    TSepiShortStringType, TSepiStringType
  ]);
end.

