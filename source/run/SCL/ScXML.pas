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

unit ScXML;
{$i ..\..\source\Sepi.inc}
interface

uses

  SysUtils, Classes, ActiveX, ScConsts,
  {$IFDEF FPC}
  XMLRead, XMLWrite, DOM
  {$ELSE}
  msxml
  {$ENDIF}
  ;
const
  /// Caractères de la base 64
  Base64Chars: AnsiString =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  /// Caractère spécial d'absence de données en fin de base 64
  Base64NoDataChar: AnsiChar = '=';

procedure Base64Encode(Input, Output: TStream);
procedure Base64Decode(Input, Output: TStream);

(* TODO FPC
function LoadXMLDocumentFromStream(Stream: TStream): IXMLDOMDocument;
procedure SaveXMLDocumentToStream(const Document: IXMLDOMDocument;
  Stream: TStream; Indent: Boolean = True);

function LoadXMLDocumentFromFile(const FileName: TFileName): IXMLDOMDocument;
procedure SaveXMLDocumentToFile(const Document: IXMLDOMDocument;
  const FileName: TFileName; Indent: Boolean = True);

procedure CreateXMLHeaderIfNotExists(const Document: IXMLDOMDocument);
function IndentXMLDocument(const Document: IXMLDOMDocument): IXMLDOMDocument;
*)

implementation

var
  ByteToBase64: array[Byte] of Byte;
  Base64ToByte: array[Byte] of Byte;
  Base64NoDataByte: Byte;

procedure InitGlobalVars;
var
  I: Integer;
begin
  for I := 0 to 63 do
  begin
    ByteToBase64[I] := Ord(Base64Chars[I+1]);
    Base64ToByte[ByteToBase64[I]] := I;
  end;

  Base64NoDataByte := Ord(Base64NoDataChar);
end;

function Base64EncodeChunk(InChunk: LongWord; ChunkSize: Integer = 3): LongWord;
var
  InRec: LongRec absolute InChunk;
  OutRec: LongRec absolute Result;
  Temp: Byte;
  I: Integer;
begin
  Temp := InRec.Bytes[0];
  InRec.Bytes[0] := InRec.Bytes[2];
  InRec.Bytes[2] := Temp;

  for I := 0 to 3 do
  begin
    OutRec.Bytes[3-I] := ByteToBase64[InChunk and 63];
    InChunk := InChunk shr 6;
  end;

  for I := ChunkSize+1 to 3 do
    OutRec.Bytes[I] := Base64NoDataByte;
end;

function Base64DecodeChunk(InChunk: LongWord; out ChunkSize: Integer): LongWord;
var
  InRec: LongRec absolute InChunk;
  OutRec: LongRec absolute Result;
  Temp: Byte;
  I: Integer;
begin
  if InRec.Bytes[2] = Base64NoDataByte then
    ChunkSize := 1
  else if InRec.Bytes[3] = Base64NoDataByte then
    ChunkSize := 2
  else
    ChunkSize := 3;

  Result := 0;
  for I := 3 downto 0 do
  begin
    Result := Result shl 6;
    Result := Result or Base64ToByte[InRec.Bytes[3-I]];
  end;

  Temp := OutRec.Bytes[0];
  OutRec.Bytes[0] := OutRec.Bytes[2];
  OutRec.Bytes[2] := Temp;
end;

{*
  Encode un flux en base 64
  @param Input    Flux à encoder
  @param Output   En sortie : flux encodé en base 64
*}
procedure Base64Encode(Input, Output: TStream);
var
  ChunkSize: Integer;
  InChunk, OutChunk: LongWord;
begin
  while True do
  begin
    InChunk := 0;
    ChunkSize := Input.Read(InChunk, 3);
    if ChunkSize = 0 then
      Break;
    OutChunk := Base64EncodeChunk(InChunk, ChunkSize);
    Output.WriteBuffer(OutChunk, 4);
  end;
end;

{*
  Décode un flux en base 64
  @param Input    Flux à décoder
  @param Output   En sortie : flux décodé
*}
procedure Base64Decode(Input, Output: TStream);
var
  ChunkSize: Integer;
  InChunk, OutChunk: LongWord;
begin
  while Input.Read(InChunk, 4) > 0 do
  begin
    OutChunk := Base64DecodeChunk(InChunk, ChunkSize);
    Output.WriteBuffer(OutChunk, ChunkSize);
  end;
end;

(* TODO FPC
{*
  Charge un document XML depuis un flux
  @param Stream   Flux source
  @return Document XML chargé
*}
function LoadXMLDocumentFromStream(Stream: TStream): IXMLDOMDocument;
var
  StreamAdapter: IStream;
begin
  Result := CoDOMDocument.Create;
  Result.async := False;

  StreamAdapter := TStreamAdapter.Create(Stream);
  if not Result.load(StreamAdapter) then
    raise EInOutError.Create(SCantLoadXMLDocument);
end;

{*
  Enregistre un document XML dans un flux
  @param Document   Document XML à enregistrer
  @param Stream     Flux dans lequel enregistrer le document
*}
procedure SaveXMLDocumentToStream(const Document: IXMLDOMDocument;
  Stream: TStream; Indent: Boolean = True);
var
  ADocument: IXMLDOMDocument;
  StreamAdapter: IStream;
begin
  if Indent then
    ADocument := IndentXMLDocument(Document)
  else
    ADocument := Document;

  CreateXMLHeaderIfNotExists(ADocument);

  StreamAdapter := TStreamAdapter.Create(Stream);
  ADocument.save(StreamAdapter);
end;

{*
  Charge un document XML depuis un fichier
  @param FileName   Nom du fichier source
  @return Document XML chargé
*}
function LoadXMLDocumentFromFile(const FileName: TFileName): IXMLDOMDocument;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := LoadXMLDocumentFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{*
  Enregistre un document XML dans un fichier
  @param Document   Document XML à enregistrer
  @param FileName   Nom du fichier destination
*}
procedure SaveXMLDocumentToFile(const Document: IXMLDOMDocument;
  const FileName: TFileName; Indent: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveXMLDocumentToStream(Document, Stream, Indent);
  finally
    Stream.Free;
  end;
end;

{*
  Crée le header XML pour un document XML s'il n'est pas déjà présent
  @param Document   Document pour lequel créer le header
*}
procedure CreateXMLHeaderIfNotExists(const Document: IXMLDOMDocument);
const
  XMLHeaderTarget = 'xml';
  XMLHeaderData = 'version="1.0" encoding="UTF-8"';
begin
  if not Supports(Document.childNodes[0], IXMLDOMProcessingInstruction) then
  begin
    Document.insertBefore(
      Document.createProcessingInstruction(XMLHeaderTarget, XMLHeaderData),
      Document.childNodes[0]);
  end;
end;

{*
  Indente un document XML
  @param Document   Document XML à indenter
  @return Document indenté
*}
function IndentXMLDocument(const Document: IXMLDOMDocument): IXMLDOMDocument;
const
  IndentStylesheetCode =
    '<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" '+
      'version="1.0">'#10+
    '  <xsl:output method="xml" encoding="UTF-8" indent="yes"/>'#10+
    '  <xsl:template match="@* | node()">'#10+
    '    <xsl:copy>'#10+
    '      <xsl:apply-templates select="@* | node()"/>'#10+
    '   </xsl:copy>'#10+
    '  </xsl:template>'#10+
    '</xsl:stylesheet>'#10;
var
  IndentStylesheet: IXMLDOMDocument;
begin
  IndentStylesheet := CoDOMDocument.Create;
  IndentStylesheet.async := False;
  IndentStylesheet.loadXML(IndentStylesheetCode);

  Result := CoDOMDocument.Create;
  Result.async := False;

  Document.transformNodeToObject(IndentStylesheet, Result);
end;
 *)
initialization
  InitGlobalVars;
end.

