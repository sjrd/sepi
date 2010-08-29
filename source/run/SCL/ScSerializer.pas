{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  S�bastien Doeraene
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
  Routines de s�rialisation de donn�es non classes
  @author sjrd
  @version 1.0
*}
unit ScSerializer;

interface

uses
  SysUtils, Classes, TypInfo, ScTypInfo;

resourcestring
  SCantSerializeType = 'Impossible de s�rialiser le type %s';

type
  ESerializerError = class(Exception);

procedure WriteDataToStream(Stream: TStream; const Data; Size: Integer;
  TypeInfo: PTypeInfo); overload;
procedure WriteDataToStream(Stream: TStream; const Data;
  TypeInfo: PTypeInfo); overload;

procedure ReadDataFromStream(Stream: TStream; var Data; Size: Integer;
  TypeInfo: PTypeInfo); overload;
procedure ReadDataFromStream(Stream: TStream; var Data;
  TypeInfo: PTypeInfo); overload;

implementation

uses
  ScCompilerMagic;

{$IFDEF UNICODE}
{*
  �crit une cha�ne de caract�res Unicode dans un flux
  @param Stream   Flux de destination
  @param Data     Source
*}
procedure WriteUnicodeStringToStream(Stream: TStream;
  const Data: UnicodeString);
var
  Len: Integer;
begin
  Len := Length(Data);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Data[1], Len * SizeOf(Char));
end;

{*
  Lit une cha�ne de caract�res Unicode depuis un flux
  @param Stream   Flux source
  @param Data     Destination
*}
procedure ReadUnicodeStringFromStream(Stream: TStream; var Data: UnicodeString);
var
  Len: Integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Data, Len);
  Stream.ReadBuffer(Data[1], Len * SizeOf(Char));
end;
{$ENDIF}

{*
  �crit une cha�ne de caract�res dans un flux
  @param Stream   Flux de destination
  @param Data     Source
*}
procedure WriteLongStringToStream(Stream: TStream; const Data: LongString);
{$IFDEF UNICODE}
begin
  WriteUnicodeStringToStream(Stream, UnicodeString(Data));
end;
{$ELSE}
var
  Len: Integer;
begin
  Len := Length(Data);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Data[1], Len * SizeOf(AnsiChar));
end;
{$ENDIF}

{$IFDEF UNICODE}
{*
  Lit une cha�ne de caract�res depuis un flux
  @param Stream     Flux source
  @param Data       Destination
  @param CodePage   Code page de la destination
*}
procedure ReadLongStringFromStream(Stream: TStream; var Data: LongString;
  CodePage: Word);
var
  UnicodeData: UnicodeString;
begin
  ReadUnicodeStringFromStream(Stream, UnicodeData);
  LStrFromUStr(AnsiString(Data), UnicodeData, CodePage);
end;
{$ELSE}
{*
  Lit une cha�ne de caract�res depuis un flux
  @param Stream   Flux source
  @param Data     Destination
*}
procedure ReadLongStringFromStream(Stream: TStream; var Data: LongString);
var
  Len: Integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Data, Len);
  Stream.ReadBuffer(Data[1], Len * SizeOf(AnsiChar));
end;
{$ENDIF}

{*
  �crit une cha�ne de caract�res Unicode dans un flux
  @param Stream   Flux de destination
  @param Data     Source
*}
procedure WriteWideStringToStream(Stream: TStream; const Data: WideString);
var
  Len: Integer;
begin
  Len := Length(Data);
  Stream.WriteBuffer(Len, 4);
  Stream.WriteBuffer(Data[1], Len * SizeOf(WideChar));
end;

{*
  Lit une cha�ne de caract�res Unicode depuis un flux
  @param Stream   Flux source
  @param Data     Destination
*}
procedure ReadWideStringFromStream(Stream: TStream; var Data: WideString);
var
  Len: Integer;
begin
  Stream.ReadBuffer(Len, 4);
  SetLength(Data, Len);
  Stream.ReadBuffer(Data[1], Len * SizeOf(WideChar));
end;

{*
  �crit un tableau statique dans un flux
  @param Stream     Flux de destination
  @param Data       Source
  @param TypeInfo   RTTI du type de donn�es
*}
procedure WriteArrayToStream(Stream: TStream; const Data;
  TypeInfo: PTypeInfo);
var
  TypeData: PArrayTypeData;
  I, ElSize: Integer;
begin
  TypeData := PArrayTypeData(GetTypeData(TypeInfo));
  ElSize := TypeData.Size div TypeData.ElCount;

  for I := 0 to TypeData.ElCount-1 do
  begin
    WriteDataToStream(Stream,
      Pointer(Cardinal(@Data) + I*ElSize)^, TypeData.ElType^);
  end;
end;

{*
  Lit un tableau statique depuis un flux
  @param Stream     Flux source
  @param Data       Destination
  @param TypeInfo   RTTI du type de donn�es
*}
procedure ReadArrayFromStream(Stream: TStream; var Data;
  TypeInfo: PTypeInfo);
var
  TypeData: PArrayTypeData;
  I, ElSize: Integer;
begin
  TypeData := PArrayTypeData(GetTypeData(TypeInfo));
  ElSize := TypeData.Size div TypeData.ElCount;

  for I := 0 to TypeData.ElCount-1 do
  begin
    ReadDataFromStream(Stream,
      Pointer(Cardinal(@Data) + I*ElSize)^, TypeData.ElType^);
  end;
end;

{*
  �crit un record dans un flux
  @param Stream     Flux de destination
  @param Data       Source
  @param TypeInfo   RTTI du type de donn�es
*}
procedure WriteRecordToStream(Stream: TStream; const Data;
  TypeInfo: PTypeInfo);
var
  TypeData: PRecordTypeData;
  DataPtr: Pointer;
  I, OldOffset, FieldSize: Integer;
begin
  TypeData := PRecordTypeData(GetTypeData(TypeInfo));

  DataPtr := @Data;
  OldOffset := 0;

  for I := 0 to TypeData.ManagedCount-1 do
  begin
    with TypeData.ManagedFields[I] do
    begin
      // Data which don't need initialization
      if FldOffset > OldOffset then
      begin
        Stream.WriteBuffer(DataPtr^, FldOffset-OldOffset);
        Inc(Cardinal(DataPtr), FldOffset-OldOffset);
      end;

      // Field which requires initialization
      WriteDataToStream(Stream, DataPtr^, TypeRef^);
      FieldSize := TypeSize(TypeRef^);

      // Next iteration
      Inc(Cardinal(DataPtr), FieldSize);
      OldOffset := FldOffset + FieldSize;
    end;
  end;

  // End data which don't require initialization
  if TypeData.Size > OldOffset then
    Stream.WriteBuffer(DataPtr^, TypeData.Size-OldOffset);
end;

{*
  Lit un record depuis un flux
  @param Stream     Flux source
  @param Data       Destination
  @param TypeInfo   RTTI du type de donn�es
*}
procedure ReadRecordFromStream(Stream: TStream; var Data;
  TypeInfo: PTypeInfo);
var
  TypeData: PRecordTypeData;
  DataPtr: Pointer;
  I, OldOffset, FieldSize: Integer;
begin
  TypeData := PRecordTypeData(GetTypeData(TypeInfo));

  DataPtr := @Data;
  OldOffset := 0;

  for I := 0 to TypeData.ManagedCount-1 do
  begin
    with TypeData.ManagedFields[I] do
    begin
      // Data which don't need initialization
      if FldOffset > OldOffset then
      begin
        Stream.ReadBuffer(DataPtr^, FldOffset-OldOffset);
        Inc(Cardinal(DataPtr), FldOffset-OldOffset);
      end;

      // Field which requires initialization
      ReadDataFromStream(Stream, DataPtr^, TypeRef^);
      FieldSize := TypeSize(TypeRef^);

      // Next iteration
      Inc(Cardinal(DataPtr), FieldSize);
      OldOffset := FldOffset + FieldSize;
    end;
  end;

  // End data which don't require initialization
  if TypeData.Size > OldOffset then
    Stream.ReadBuffer(DataPtr^, TypeData.Size-OldOffset);
end;

{*
  �crit un tableau dynamique dans un flux
  @param Stream     Flux de destination
  @param Data       Source
  @param TypeInfo   RTTI du type de donn�es
*}
procedure WriteDynArrayToStream(Stream: TStream; const Data;
  TypeInfo: PTypeInfo);
var
  TypeData: PTypeData;
  I, Count: Integer;
begin
  TypeData := GetTypeData(TypeInfo);

  if Pointer(Data) = nil then
  begin
    // No element
    Count := 0;
    Stream.WriteBuffer(Count, SizeOf(Integer));
  end else
  begin
    // At least 1 element
    Count := PInteger(Cardinal(Data)-SizeOf(Integer))^;
    Stream.WriteBuffer(Count, SizeOf(Integer));

    if TypeData.elType = nil then
    begin
      // Element type doesn't require initialization
      Stream.WriteBuffer(Pointer(Data)^, Count*TypeData.elSize);
    end else
    begin
      // Element type requires initialization: recursive writing
      for I := 0 to Count-1 do
      begin
        WriteDataToStream(Stream,
          Pointer(Cardinal(Data) + I*TypeData.elSize)^, TypeData.elType^);
      end;
    end;
  end;
end;

{*
  Lit un tableau dynamique depuis un flux
  @param Stream     Flux source
  @param Data       Destination
  @param TypeInfo   RTTI du type de donn�es
*}
procedure ReadDynArrayFromStream(Stream: TStream; var Data;
  TypeInfo: PTypeInfo);
var
  TypeData: PTypeData;
  I, Count: Integer;
begin
  TypeData := GetTypeData(TypeInfo);

  Stream.ReadBuffer(Count, SizeOf(Integer));

  if Count = 0 then
  begin
    // No element
    DynArrayClear(Pointer(Data), TypeInfo);
  end else
  begin
    // At least 1 element
    DynArraySetLength(Pointer(Data), TypeInfo, 1, @Count);

    if TypeData.elType = nil then
    begin
      // Element type doesn't require initialization
      Stream.ReadBuffer(Pointer(Data)^, Count*TypeData.elSize);
    end else
    begin
      // Element type requires initialization: recursive reading
      for I := 0 to Count-1 do
      begin
        ReadDataFromStream(Stream,
          Pointer(Cardinal(Data) + I*TypeData.elSize)^, TypeData.elType^);
      end;
    end;
  end;
end;

{*
  �crit des donn�es dans un flux
  WriteDataToStream peut s�rialiser correctement des cha�nes ANSI et Unicode,
  des tableaux statiques ou dynamiques, et des record. Cette routine ne peut
  pas s�rialiser les variants ni les interfaces.
  @param Stream     Flux de destination
  @param Data       Source
  @param TypeInfo   RTTI du type de donn�es
*}
procedure WriteDataToStream(Stream: TStream; const Data; Size: Integer;
  TypeInfo: PTypeInfo);
var
  Kind: TTypeKind;
begin
  if TypeInfo = nil then
    Kind := tkUnknown
  else
    Kind := TypeInfo.Kind;

  case Kind of
    tkLString: WriteLongStringToStream(Stream, LongString(Data));
    tkWString: WriteWideStringToStream(Stream, WideString(Data));
    tkArray: WriteArrayToStream(Stream, Data, TypeInfo);
    tkRecord: WriteRecordToStream(Stream, Data, TypeInfo);
    tkDynArray: WriteDynArrayToStream(Stream, Data, TypeInfo);
    tkVariant, tkInterface:
      raise ESerializerError.CreateResFmt(@SCantSerializeType,
        [TypeInfo.Name]);
    {$IFDEF UNICODE}
    tkUString: WriteUnicodeStringToStream(Stream, UnicodeString(Data));
    {$ENDIF}
  else
    Stream.WriteBuffer(Data, Size);
  end;
end;

{*
  �crit des donn�es dans un flux
  WriteDataToStream peut s�rialiser correctement des cha�nes ANSI et Unicode,
  des tableaux statiques ou dynamiques, et des record. Cette routine ne peut
  pas s�rialiser les variants ni les interfaces.
  @param Stream     Flux de destination
  @param Data       Source
  @param TypeInfo   RTTI du type de donn�es
*}
procedure WriteDataToStream(Stream: TStream; const Data; TypeInfo: PTypeInfo);
begin
  WriteDataToStream(Stream, Data, TypeSize(TypeInfo), TypeInfo);
end;

{*
  Lit des donn�es depuis un flux
  ReadDataFromStream relit des donn�es qui ont �t� �crites avec
  WriteDataToStream.
  @param Stream     Flux source
  @param Data       Destination
  @param TypeInfo   RTTI du type de donn�es
*}
procedure ReadDataFromStream(Stream: TStream; var Data; Size: Integer;
  TypeInfo: PTypeInfo);
var
  Kind: TTypeKind;
begin
  if TypeInfo = nil then
    Kind := tkUnknown
  else
    Kind := TypeInfo.Kind;

  case Kind of
    tkLString: ReadLongStringFromStream(Stream, LongString(Data)
      {$IFDEF UNICODE}, GetTypeData(TypeInfo).CodePage {$ENDIF});
    tkWString: ReadWideStringFromStream(Stream, WideString(Data));
    tkArray: ReadArrayFromStream(Stream, Data, TypeInfo);
    tkRecord: ReadRecordFromStream(Stream, Data, TypeInfo);
    tkDynArray: ReadDynArrayFromStream(Stream, Data, TypeInfo);
    tkVariant, tkInterface:
      raise ESerializerError.CreateResFmt(@SCantSerializeType,
        [TypeInfo.Name]);
    {$IFDEF UNICODE}
    tkUString: ReadUnicodeStringFromStream(Stream, UnicodeString(Data));
    {$ENDIF}
  else
    Stream.ReadBuffer(Data, Size);
  end;
end;

{*
  Lit des donn�es depuis un flux
  ReadDataFromStream relit des donn�es qui ont �t� �crites avec
  WriteDataToStream.
  @param Stream     Flux source
  @param Data       Destination
  @param TypeInfo   RTTI du type de donn�es
*}
procedure ReadDataFromStream(Stream: TStream; var Data; TypeInfo: PTypeInfo);
begin
  ReadDataFromStream(Stream, Data, TypeSize(TypeInfo), TypeInfo);
end;

end.

