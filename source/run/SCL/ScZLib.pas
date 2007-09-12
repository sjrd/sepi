{*
  Définit deux routines de compression et décompression avec ZLib
  @author sjrd
  @version 1.0
*}
unit ScZLib;

interface

uses
  Classes, ZLib;

const
  clNoComp = ZLib.clNone;         /// Pas de compression
  clFastestComp = ZLib.clFastest; /// Compression la plus rapide
  clDefaultComp = ZLib.clDefault; /// Compression par défaut
  clMaxComp = ZLib.clMax;         /// Compression maximale

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
  if Dest = Stream then Dest := nil;

  // Si Dest vaut nil, on crée un flux temporaire de destination
  if Dest = nil then Destination := TMemoryStream.Create else
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
  Buffer: array [0..1023] of Byte;
  Copied: Integer;
begin
  if Dest = Stream then Dest := nil;

  // Si Dest vaut nil, on crée un flux temporaire de destination
  if Dest = nil then Destination := TMemoryStream.Create else
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

