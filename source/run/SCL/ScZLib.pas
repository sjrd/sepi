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
  Définit deux routines de compression et décompression avec ZLib
  @author sjrd
  @version 1.0
*}
unit ScZLib;
{$i ..\..\source\Sepi.inc}
interface

uses
  Classes,
  {$IFDEF FPC}
  zstream
  {$ELSE}
  ZLib
  {$ENDIF}
  ;

const
  {$IFDEF FPC}
  clNoComp = zstream.cldefault;       /// Pas de compression
  clFastestComp = zstream.clfastest;  /// Compression la plus rapide
  clDefaultComp = zstream.cldefault;  /// Compression par défaut
  clMaxComp = zstream.clmax;          /// Compression maximale
  {$ELSE}
  clNoComp = ZLib.clNone;         /// Pas de compression
  clFastestComp = ZLib.clFastest; /// Compression la plus rapide
  clDefaultComp = ZLib.clDefault; /// Compression par défaut
  clMaxComp = ZLib.clMax;         /// Compression maximale
  {$ENDIF}

procedure CompressStream(Stream: TStream; Dest: TStream = nil;
  CompressionLevel: TCompressionLevel = clDefaultComp);
procedure DecompressStream(Stream: TStream; Dest: TStream = nil);

implementation

{*
  Compresse un flux avec la bibliothèque ZLib
  @param Stream             Flux à compresser
  @param Dest               Flux de destination (ou nil pour Stream, plus lent)
  @param CompressionLevel   Niveau de compression
*}
procedure CompressStream(Stream: TStream; Dest: TStream = nil;
  CompressionLevel: TCompressionLevel = clDefaultComp);
var
  Destination: TStream;
begin
  if Dest = Stream then
    Dest := nil;

  // Si Dest vaut nil, on crée un flux temporaire de destination
  if Dest = nil then
    Destination := TMemoryStream.Create
  else
  begin
    Destination := Dest;
    Destination.Position := 0;
    Destination.Size := 0;
  end;

  // Création, utilisation et libération du flux de compression
  with TCompressionStream.Create(CompressionLevel, Destination) do
  try
    CopyFrom(Stream, 0);
  finally
    Free;
  end;

  // Si Dest vaut nil, on recopie Destination dans Stream
  if Dest = nil then
  begin
    Stream.Position := 0;
    Stream.Size := 0;
    Stream.CopyFrom(Destination, 0);
    Destination.Free;
  end;
end;

{*
  Décompresse un flux avec la bibliothèque ZLib
  @param Stream   Flux à décompresser
  @param Dest     Flux de destination (ou nil pour Stream, plus lent)
*}
procedure DecompressStream(Stream: TStream; Dest: TStream = nil);
var
  Destination: TStream;
  Buffer: array[0..1023] of Byte;
  Copied: Integer;
begin
  if Dest = Stream then
    Dest := nil;

  // Si Dest vaut nil, on crée un flux temporaire de destination
  if Dest = nil then
    Destination := TMemoryStream.Create
  else
  begin
    Destination := Dest;
    Destination.Position := 0;
    Destination.Size := 0;
  end;

  // Création, utilisation et libération du flux de décompression
  with TDecompressionStream.Create(Stream) do
  try
    Position := 0;
    repeat
      Copied := Read(Buffer, 1024);
      Destination.Write(Buffer, Copied);
    until Copied < 1024;
  finally
    Free;
  end;

  // Si Dest vaut nil, on recopie Destination dans Stream
  if Dest = nil then
  begin
    Stream.Position := 0;
    Stream.Size := 0;
    Stream.CopyFrom(Destination, 0);
  end;
end;

end.

