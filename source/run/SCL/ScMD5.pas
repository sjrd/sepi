{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of the SCL (Sepi Code Library), which is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.

Linking this library -the SCL- statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and conditions
of the GNU General Public License cover the whole combination.

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
  Gestion de hashes MD5
  @author RSA Data Security, Inc.
  @version 1.0
*}
unit ScMD5;

interface

uses
  Windows, SysUtils, Classes;

type
  PMD5Digest = ^TMD5Digest; /// Pointeur vers un TMD5Digest

  {*
    Représente un hash MD5
  *}
  TMD5Digest = record
    case Integer of
      0: (A, B, C, D: Longint);
      1: (V: array [0..15] of Byte);
  end;

function MD5String(const S: string): TMD5Digest;
function MD5File(const FileName: TFileName): TMD5Digest;
function MD5Stream(const Stream: TStream): TMD5Digest;
function MD5Buffer(const Buffer; Size: Integer): TMD5Digest;

function MD5DigestToStr(const Digest: TMD5Digest): string;
function StrToMD5Digest(Str: string): TMD5Digest;
function MD5DigestCompare(const Digest1, Digest2: TMD5Digest): Boolean;

implementation

{
  Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
  rights reserved.

  License to copy and use this software is granted provided that it
  is identified as the "RSA Data Security, Inc. MD5 Message-Digest
  Algorithm" in all material mentioning or referencing this software
  or this function.

  License is also granted to make and use derivative works provided
  that such works are identified as "derived from the RSA Data
  Security, Inc. MD5 Message-Digest Algorithm" in all material
  mentioning or referencing the derived work.

  RSA Data Security, Inc. makes no representations concerning either
  the merchantability of this software or the suitability of this
  software for any particular purpose. It is provided "as is"
  without express or implied warranty of any kind.

  These notices must be retained in any copies of any part of this
  documentation and/or software.
}

type
  UINT4 = LongWord;

  PArray4UINT4 = ^TArray4UINT4;
  TArray4UINT4 = array [0..3] of UINT4;
  PArray2UINT4 = ^TArray2UINT4;
  TArray2UINT4 = array [0..1] of UINT4;
  PArray16Byte = ^TArray16Byte;
  TArray16Byte = array [0..15] of Byte;
  PArray64Byte = ^TArray64Byte;
  TArray64Byte = array [0..63] of Byte;

  PByteArray = ^TByteArray;
  TByteArray = array [0..0] of Byte;

  PUINT4Array = ^TUINT4Array;
  TUINT4Array = array [0..0] of UINT4;

  PMD5Context = ^TMD5Context;
  TMD5Context = record
    State: TArray4UINT4;
    Count: TArray2UINT4;
    Buffer: TArray64Byte;
  end;

const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;
  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;
  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;
  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;

var
  Padding: TArray64Byte =
    ($80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);


function _F(X, Y, Z: UINT4): UINT4;
begin
  Result := (((X) and (Y)) or ((not X) and (Z)));
end;

function _G(X, Y, Z: UINT4): UINT4;
begin
  Result := (((X) and (Z)) or ((Y) and (not Z)));
end;

function _H(X, Y, Z: UINT4): UINT4;
begin
  Result := ((X) xor (Y) xor (Z));
end;

function _I(X, Y, Z: UINT4): UINT4;
begin
  Result := ((Y) xor ((X) or (not Z)));
end;

function ROTATE_LEFT(X, N: UINT4): UINT4;
begin
  Result := (((X) shl (N)) or ((X) shr (32-(N))));
end;

procedure FF(var A: UINT4; B, C, D, X, S, AC: UINT4);
begin
  A := A + _F(B, C, D) + X + AC;
  A := ROTATE_LEFT(A, S);
  A := A + B;
end;

procedure GG(var A: UINT4; B, C, D, X, S, AC: UINT4);
begin
  A := A + _G(B, C, D) + X + AC;
  A := ROTATE_LEFT(A, S);
  A := A + B;
end;

procedure HH(var A: UINT4; B, C, D, X, S, AC: UINT4);
begin
  A := A + _H(B, C, D) + X + AC;
  A := ROTATE_LEFT(A, S);
  A := A + B;
end;

procedure II(var A: UINT4; B, C, D, X, S, AC: UINT4);
begin
  A := A + _I(B, C, D) + X + AC;
  A := ROTATE_LEFT(A, S);
  A := A + B;
end;

procedure MD5Encode(Output: PByteArray; Input: PUINT4Array; Len: LongWord);
var
  I, J: LongWord;
begin
  J := 0;
  I := 0;
  while J < Len do
  begin
    Output[J] := Byte(Input[I] and $FF);
    Output[J+1] := Byte((Input[I] shr 8) and $FF);
    Output[J+2] := Byte((Input[I] shr 16) and $FF);
    Output[J+3] := Byte((Input[I] shr 24) and $FF);
    Inc(J, 4);
    Inc(I);
  end;
end;

procedure MD5Decode(Output: PUINT4Array; Input: PByteArray; Len: LongWord);
var
  I, J: LongWord;
begin
  J := 0;
  I := 0;
  while J < Len do
  begin
    Output[I] := UINT4(Input[J]) or (UINT4(Input[J+1]) shl 8) or
      (UINT4(Input[J+2]) shl 16) or (UINT4(Input[J+3]) shl 24);
    Inc(J, 4);
    Inc(I);
  end;
end;

procedure MD5_memcpy(Output: PByteArray; Input: PByteArray; Len: LongWord);
begin
  Move(Input^, Output^, Len);
end;

procedure MD5_memset(Output: PByteArray; Value: Integer; Len: LongWord);
begin
  FillChar(Output^, Len, Byte(Value));
end;

procedure MD5Transform(State: PArray4UINT4; Buffer: PArray64Byte);
var
  A, B, C, D: UINT4;
  X: array[0..15] of UINT4;
begin
  A := State[0];
  B := State[1];
  C := State[2];
  D := State[3];
  MD5Decode(PUINT4Array(@X), PByteArray(Buffer), 64);

  FF(A, B, C, D, X[0], S11, $D76AA478);
  FF(D, A, B, C, X[1], S12, $E8C7B756);
  FF(C, D, A, B, X[2], S13, $242070DB);
  FF(B, C, D, A, X[3], S14, $C1BDCEEE);
  FF(A, B, C, D, X[4], S11, $F57C0FAF);
  FF(D, A, B, C, X[5], S12, $4787C62A);
  FF(C, D, A, B, X[6], S13, $A8304613);
  FF(B, C, D, A, X[7], S14, $FD469501);
  FF(A, B, C, D, X[8], S11, $698098D8);
  FF(D, A, B, C, X[9], S12, $8B44F7AF);
  FF(C, D, A, B, X[10], S13, $FFFF5BB1);
  FF(B, C, D, A, X[11], S14, $895CD7BE);
  FF(A, B, C, D, X[12], S11, $6B901122);
  FF(D, A, B, C, X[13], S12, $FD987193);
  FF(C, D, A, B, X[14], S13, $A679438E);
  FF(B, C, D, A, X[15], S14, $49B40821);

  GG(A, B, C, D, X[1], S21, $F61E2562);
  GG(D, A, B, C, X[6], S22, $C040B340);
  GG(C, D, A, B, X[11], S23, $265E5A51);
  GG(B, C, D, A, X[0], S24, $E9B6C7AA);
  GG(A, B, C, D, X[5], S21, $D62F105D);
  GG(D, A, B, C, X[10], S22, $02441453);
  GG(C, D, A, B, X[15], S23, $D8A1E681);
  GG(B, C, D, A, X[4], S24, $E7D3FBC8);
  GG(A, B, C, D, X[9], S21, $21E1CDE6);
  GG(D, A, B, C, X[14], S22, $C33707D6);
  GG(C, D, A, B, X[3], S23, $F4D50D87);

  GG(B, C, D, A, X[8], S24, $455A14ED);
  GG(A, B, C, D, X[13], S21, $A9E3E905);
  GG(D, A, B, C, X[2], S22, $FCEFA3F8);
  GG(C, D, A, B, X[7], S23, $676F02D9);
  GG(B, C, D, A, X[12], S24, $8D2A4C8A);

  HH(A, B, C, D, X[5], S31, $FFFA3942);
  HH(D, A, B, C, X[8], S32, $8771F681);
  HH(C, D, A, B, X[11], S33, $6D9D6122);
  HH(B, C, D, A, X[14], S34, $FDE5380C);
  HH(A, B, C, D, X[1], S31, $A4BEEA44);
  HH(D, A, B, C, X[4], S32, $4BDECFA9);
  HH(C, D, A, B, X[7], S33, $F6BB4B60);
  HH(B, C, D, A, X[10], S34, $BEBFBC70);
  HH(A, B, C, D, X[13], S31, $289B7EC6);
  HH(D, A, B, C, X[0], S32, $EAA127FA);
  HH(C, D, A, B, X[3], S33, $D4EF3085);
  HH(B, C, D, A, X[6], S34, $04881D05);
  HH(A, B, C, D, X[9], S31, $D9D4D039);
  HH(D, A, B, C, X[12], S32, $E6DB99E5);
  HH(C, D, A, B, X[15], S33, $1FA27CF8);
  HH(B, C, D, A, X[2], S34, $C4AC5665);

  II(A, B, C, D, X[0], S41, $F4292244);
  II(D, A, B, C, X[7], S42, $432AFF97);
  II(C, D, A, B, X[14], S43, $AB9423A7);
  II(B, C, D, A, X[5], S44, $FC93A039);
  II(A, B, C, D, X[12], S41, $655B59C3);
  II(D, A, B, C, X[3], S42, $8F0CCC92);
  II(C, D, A, B, X[10], S43, $FFEFF47D);
  II(B, C, D, A, X[1], S44, $85845DD1);
  II(A, B, C, D, X[8], S41, $6FA87E4F);
  II(D, A, B, C, X[15], S42, $FE2CE6E0);
  II(C, D, A, B, X[6], S43, $A3014314);
  II(B, C, D, A, X[13], S44, $4E0811A1);
  II(A, B, C, D, X[4], S41, $F7537E82);
  II(D, A, B, C, X[11], S42, $BD3AF235);
  II(C, D, A, B, X[2], S43, $2AD7D2BB);
  II(B, C, D, A, X[9], S44, $EB86D391);

  Inc(State[0], A);
  Inc(State[1], B);
  Inc(State[2], C);
  Inc(State[3], D);

  MD5_memset(PByteArray(@X), 0, SizeOf(X));
end;


procedure MD5Init(var Context: TMD5Context);
begin
  FillChar(Context, SizeOf(Context), 0);
  Context.State[0] := $67452301;
  Context.State[1] := $EFCDAB89;
  Context.State[2] := $98BADCFE;
  Context.State[3] := $10325476;
end;

procedure MD5Update(var Context: TMD5Context; Input: PByteArray;
  InputLen: LongWord);
var
  I, Index, PartLen: LongWord;
begin
  Index := LongWord((Context.Count[0] shr 3) and $3F);
  Inc(Context.Count[0], UINT4(InputLen) shl 3);
  if Context.Count[0] < UINT4(InputLen) shl 3 then
    Inc(Context.Count[1]);
  Inc(Context.Count[1], UINT4(InputLen) shr 29);
  PartLen := 64 - Index;
  if InputLen >= PartLen then
  begin
    MD5_memcpy(PByteArray(@Context.Buffer[Index]), Input, PartLen);
    MD5Transform(@Context.state, @Context.Buffer);
    I := PartLen;
    while I + 63 < InputLen do
    begin
      MD5Transform(@Context.State, PArray64Byte(@Input[I]));
      Inc(I, 64);
    end;
    Index := 0;
  end else
    I := 0;
  MD5_memcpy(PByteArray(@Context.buffer[Index]), PByteArray(@Input[I]),
    InputLen - I);
end;

procedure MD5Final(var Digest: TMD5Digest; var Context: TMD5Context);
var
  Bits: array [0..7] of Byte;
  Index, PadLen: LongWord;
begin
  MD5Encode(PByteArray(@Bits), PUINT4Array(@Context.Count), 8);
  Index := LongWord((Context.Count[0] shr 3) and $3F);
  if Index < 56 then
    PadLen := 56 - Index
  else
    PadLen := 120 - Index;
  MD5Update(Context, PByteArray(@Padding), PadLen);
  MD5Update(Context, PByteArray(@Bits), 8);
  MD5Encode(PByteArray(@Digest), PUINT4Array(@Context.State), 16);
  MD5_memset(PByteArray(@Context), 0, SizeOf(Context));
end;

{*
  Convertit un hash MD5 en chaîne de caractères
  @param Digest   Hash MD5 à convertir
  @return Représentation sous forme de chaîne du hash Digest
*}
function MD5DigestToStr(const Digest: TMD5Digest): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to 15 do
    Result := Result + IntToHex(Digest.V[I], 2);
end;

{*
  Convertit une chaîne de caractères en hash MD5
  @param Str   Chaîne de caractères à convertir
  @return Hash MD5 représenté par la chaîne Str
*}
function StrToMD5Digest(Str: string): TMD5Digest;
var
  I: Integer;
begin
  for I := 0 to 15 do
    Result.V[I] := StrToInt('$'+Str[I*2+1]+Str[I*2+2]);
end;

{*
  Calcule le hash MD5 d'une chaîne de caractères
  @param S   Chaîne de caractères
  @return Hash MD5 de la chaîne S
*}
function MD5String(const S: string): TMD5Digest;
begin
  Result := MD5Buffer(PChar(S)^, Length(S));
end;

{*
  Calcule le hash MD5 d'un fichier
  @param FileName   Nom du fichier
  @return Hash MD5 du fichier FileName
*}
function MD5File(const FileName: TFileName): TMD5Digest;
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := MD5Stream(F);
  finally
    F.Free;
  end;
end;

{*
  Calcule le hash MD5 d'un flux
  @param Stream   Flux
  @return Hash MD5 du flux Stream
*}
function MD5Stream(const Stream: TStream): TMD5Digest;
var
  Context: TMD5Context;
  Buffer: array[0..4095] of Byte;
  Size: Integer;
  ReadBytes: Integer;
  TotalBytes: Integer;
  SavePos: Integer;
begin
  MD5Init(Context);
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  try
    Stream.Seek(0, soFromBeginning);
    repeat
      ReadBytes := Stream.Read(Buffer, SizeOf(Buffer));
      Inc(TotalBytes, ReadBytes);
      MD5Update(Context, @Buffer, ReadBytes);
    until (ReadBytes = 0) or (TotalBytes = Size);
  finally
    Stream.Seek(SavePos, soFromBeginning);
  end;
  MD5Final(Result, Context);
end;

{*
  Calcule le hash MD5 d'un buffer en mémoire
  @param Buffer   Adresse du buffer
  @param Size     Taille du buffer
  @return Hash MD5 du buffer
*}
function MD5Buffer(const Buffer; Size: Integer): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PByteArray(@Buffer), Size);
  MD5Final(Result, Context);
end;

{*
  Compare deux hashes MD5
  @param Digest1   Premier hash MD5
  @param Digest2   Second hash MD5
  @return True si les deux hashes sont identiques, False sinon
*}
function MD5DigestCompare(const Digest1, Digest2: TMD5Digest): Boolean;
begin
  Result := False;
  if Digest1.A <> Digest2.A then
    Exit;
  if Digest1.B <> Digest2.B then
    Exit;
  if Digest1.C <> Digest2.C then
    Exit;
  if Digest1.D <> Digest2.D then
    Exit;
  Result := True;
end;

end.

