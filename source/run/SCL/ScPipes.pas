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
  Classes de gestion de pipes (tuyaux)
  @author sjrd
  @version 1.0
*}
unit ScPipes;
{$i ..\..\source\Sepi.inc}
interface

uses
  Windows, SysUtils, Classes, SyncObjs, ScClasses, ScCoroutines;

resourcestring
  SBadBufferSize = 'Taille de buffer incorrecte (%d)';
  SInvalidStreamOp = 'Op�ration de flux incorrecte';
  SBufferIsntThatLarge = 'Le buffer ne contient pas %d octets';

const
  /// Taille de buffer par d�faut
  DefaultBufferSize = 4096;

type
  /// Error de un flux de transformation
  ETransformationStreamError = class(EInOutError);

  /// M�thode de transformation d'un flux
  TTransformStreamMethod = procedure(Input, Output: TStream) of object;

  {*
    Flux cr�� par transformation � la vol�e d'un autre flux
    @author sjrd
    @version 1.0
  *}
  TTransformationStream = class(TStream)
  private
    FTransformMethod: TTransformStreamMethod; /// M�thode de transformation
    FInput: TStream;                          /// Flux entrant
    FOwnsInput: Boolean;                      /// Poss�de le flux entrant

    FPosition: Int64; /// Position du flux
  protected
    procedure IncPosition(const Value: Int64);
    procedure TransformStream; dynamic;

    property TransformMethod: TTransformStreamMethod read FTransformMethod;
    property Input: TStream read FInput;
  public
    constructor Create(ATransformMethod: TTransformStreamMethod;
      AInput: TStream; AOwnsInput: Boolean = False); overload;
    constructor Create(AInput: TStream = nil;
      AOwnsInput: Boolean = False); overload;
    destructor Destroy; override;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    class procedure Error(const Msg: string;
      Data: Integer = 0); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer = 0); overload;
  end;

  {*
    Flux de transformation dont le flot de contr�le est une coroutine
    @author sjrd
    @version 1.0
  *}
  TCoroutineTransformationStream = class(TTransformationStream)
  private
    FCoroutine: TCoroutine; /// Coroutine interne

    FBuffer: Pointer;     /// Buffer de donn�es entre les coroutines
    FBufferSize: Integer; /// Taille du buffer

    procedure PrivTransformStream(Coroutine: TCoroutine);
  protected
    property Coroutine: TCoroutine read FCoroutine;
  public
    constructor Create(ATransformMethod: TTransformStreamMethod;
      AInput: TStream; AOwnsInput: Boolean = False); overload;
    constructor Create(AInput: TStream = nil;
      AOwnsInput: Boolean = False); overload;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  {*
    Flux de transformation dont le flot de contr�le est un thread
    @author sjrd
    @version 1.0
  *}
  TThreadTransformationStream = class(TTransformationStream)
  private
    FThread: TMethodThread;         /// Thread interne

    {$IFDEF FPC}
    FWrittenSomethingEvent: TSimpleEvent; /// Quelque chose a �t� �crit
    FReadSomethingEvent: TSimpleEvent;    /// Quelque chose a �t� lu
    {$ELSE}
    FWrittenSomethingEvent: TEvent; /// Quelque chose a �t� �crit
    FReadSomethingEvent: TEvent;    /// Quelque chose a �t� lu
    {$ENDIF}

    FWritePosition: Int64;    /// Position en �criture
    FBufferSize: Integer;     /// Taille du buffer
    FBuffer: Pointer;         /// Buffer de donn�es entre les coroutines
    FBufferCount: Integer;    /// Taille des donn�es valides dans le buffer
    FBufferWritePos: Integer; /// Position dans le buffer
    FBufferReadPos: Integer;  /// Position de lecture dans le buffer
    FTerminated: Boolean;     /// True lorsque le thread est termin�

    procedure PrivTransformStream(Thread: TMethodThread);
    procedure ThreadTerminated(Sender: TObject);
  protected
    procedure IncWritePosition(const Value: Int64);
    procedure InternalRead(Dest: Pointer; Count: Integer);
    procedure InternalWrite(Source: Pointer; Count: Integer);
    procedure RaiseFatalException;

    property Thread: TMethodThread read FThread;

    {$IFDEF FPC}
    property WrittenSomethingEvent: TSimpleEvent read FWrittenSomethingEvent;
    property ReadSomethingEvent: TSimpleEvent read FReadSomethingEvent;
    {$ELSE}
    property WrittenSomethingEvent: TEvent read FWrittenSomethingEvent;
    property ReadSomethingEvent: TEvent read FReadSomethingEvent;
    {$ENDIF}

    property BufferSize: Integer read FBufferSize;
    property BufferCount: Integer read FBufferCount;
    property Terminated: Boolean read FTerminated;
  public
    constructor Create(ATransformMethod: TTransformStreamMethod;
      AInput: TStream; AOwnsInput: Boolean = False;
      ABufferSize: Integer = DefaultBufferSize); overload;
    constructor Create(AInput: TStream = nil; AOwnsInput: Boolean = False;
      ABufferSize: Integer = DefaultBufferSize); overload;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

procedure CopyStream(Source, Dest: TStream);

function MakeCoroutineTransformStream(Input: TStream;
  const Transformations: array of TTransformStreamMethod;
  FreeInput: Boolean = True): TStream;
procedure MultiCoroutineTransformStream(Input, Output: TStream;
  const Transformations: array of TTransformStreamMethod;
  FreeInput: Boolean = False);

function MakeThreadTransformStream(Input: TStream;
  const Transformations: array of TTransformStreamMethod;
  const BufferSizes: array of Integer; FreeInput: Boolean = True): TStream;
procedure MultiThreadTransformStream(Input, Output: TStream;
  const Transformations: array of TTransformStreamMethod;
  const BufferSizes: array of Integer; FreeInput: Boolean = False); overload;
procedure MultiThreadTransformStream(Input, Output: TStream;
  const Transformations: array of TTransformStreamMethod;
  BufferSize: Integer = DefaultBufferSize;
  FreeInput: Boolean = False); overload;

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  Copie un flux dans un autre
  CopyStream, contrairement � TStream.CopyFrom, ne d�place jamais le curseur de
  la source, et ne suppose pas que la propri�t� Source.Size est accessible.
  @param Source   Flux source
  @param Dest     Flux destination
*}
procedure CopyStream(Source, Dest: TStream);
const
  BufferSize = 1024;
var
  Buffer: array[0..BufferSize-1] of Byte;
  Amount: Longint;
begin
  repeat
    Amount := Source.Read(Buffer, BufferSize);
    if Amount > 0 then
      Dest.WriteBuffer(Buffer, Amount);
  until Amount < BufferSize;
end;

{*
  Ajoute une valeur enti�re � une autre de fa�on atomique
  @param Value      Valeur � modifier
  @param AddParam   Valeur � ajouter � Value
*}
procedure InterlockedAdd(var Value: Integer; AddParam: Integer);
begin
//asm
        { ->    EAX     Pointer to Value
                EDX     AddParam         }
// TODO LAZ   LOCK ADD     [EAX],EDX
end;

{*
  Construit un flux par transformations multiples avec des coroutines
  Les longueurs des tableaux Transformations et StackSizes doivent �tre
  identiques.
  @param Input             Flux d'entr�e de la premi�re transformation
  @param Transformations   Tableau de transformations � appliquer
  @param FreeInput         True lib�re le flux d'entr�e, m�me en cas d'exception
  @return Flux de transformations multiples
*}
function MakeCoroutineTransformStream(Input: TStream;
  const Transformations: array of TTransformStreamMethod;
  FreeInput: Boolean = True): TStream;
var
  I, Count: Integer;
begin
  try
    Count := Length(Transformations);

    for I := 0 to Count-1 do
    begin
      Input := TCoroutineTransformationStream.Create(
        Transformations[I], Input, FreeInput);
      FreeInput := True;
    end;

    Result := Input;
    FreeInput := False;
  finally
    if FreeInput then
      Input.Free;
  end;
end;

{*
  Transforme plusieurs fois un flux en contr�lant le flot avec des coroutines
  Les longueurs des tableaux Transformations et StackSizes doivent �tre
  identiques.
  @param Input             Flux d'entr�e de la premi�re transformation
  @param Ouput             Flux de sortie de la derni�re transformation
  @param Transformations   Tableau de transformations � appliquer
  @param FreeInput         True lib�re le flux d'entr�e, m�me en cas d'exception
*}
procedure MultiCoroutineTransformStream(Input, Output: TStream;
  const Transformations: array of TTransformStreamMethod;
  FreeInput: Boolean = False);
var
  Transformed: TStream;
begin
  Transformed := MakeCoroutineTransformStream(
    Input, Transformations, FreeInput);
  try
    CopyStream(Transformed, Output);
  finally
    Transformed.Free;
  end;
end;

{*
  Construit un flux par transformations multiples avec des threads
  Les longueurs des tableaux Transformations et BufferSizes doivent �tre
  identiques.
  @param Input             Flux d'entr�e de la premi�re transformation
  @param Transformations   Tableau de transformations � appliquer
  @param BufferSizes       Tailles de buffer pour chaque transformation
  @param FreeInput         True lib�re le flux d'entr�e, m�me en cas d'exception
  @return Flux de transformations multiples
*}
function MakeThreadTransformStream(Input: TStream;
  const Transformations: array of TTransformStreamMethod;
  const BufferSizes: array of Integer; FreeInput: Boolean = True): TStream;
var
  I, Count: Integer;
begin
  try
    Count := Length(Transformations);
    Assert(Length(BufferSizes) = Count);

    for I := 0 to Count-1 do
    begin
      Input := TThreadTransformationStream.Create(
        Transformations[I], Input, FreeInput, BufferSizes[I]);
      FreeInput := True;
    end;

    Result := Input;
    FreeInput := False;
  finally
    if FreeInput then
      Input.Free;
  end;
end;

{*
  Transforme plusieurs fois un flux en contr�lant le flot avec des threads
  Les longueurs des tableaux Transformations et BufferSizes doivent �tre
  identiques.
  @param Input             Flux d'entr�e de la premi�re transformation
  @param Ouput             Flux de sortie de la derni�re transformation
  @param Transformations   Tableau de transformations � appliquer
  @param BufferSizes       Tailles de buffer pour chaque transformation
  @param FreeInput         True lib�re le flux d'entr�e, m�me en cas d'exception
*}
procedure MultiThreadTransformStream(Input, Output: TStream;
  const Transformations: array of TTransformStreamMethod;
  const BufferSizes: array of Integer; FreeInput: Boolean = False); overload;
var
  Transformed: TStream;
begin
  Transformed := MakeThreadTransformStream(
    Input, Transformations, BufferSizes, FreeInput);
  try
    CopyStream(Transformed, Output);
  finally
    Transformed.Free;
  end;
end;

{*
  Transforme plusieurs fois un flux en contr�lant le flot avec des threads
  @param Input             Flux d'entr�e de la premi�re transformation
  @param Ouput             Flux de sortie de la derni�re transformation
  @param Transformations   Tableau de transformations � appliquer
  @param BufferSize        Taille de buffer pour toutes les transformations
  @param FreeInput         True lib�re le flux d'entr�e, m�me en cas d'exception
*}
procedure MultiThreadTransformStream(Input, Output: TStream;
  const Transformations: array of TTransformStreamMethod;
  BufferSize: Integer = DefaultBufferSize;
  FreeInput: Boolean = False); overload;
var
  BufferSizes: array of Integer;
  I: Integer;
begin
  SetLength(BufferSizes, Length(Transformations));
  for I := 0 to Length(BufferSizes)-1 do
    BufferSizes[I] := BufferSize;
  MultiThreadTransformStream(
    Input, Output, Transformations, BufferSizes, FreeInput);
end;

{-----------------------------}
{ TTransformationStream class }
{-----------------------------}

{*
  Cr�e un flux par transformation
  Cette variante de Create doit �tre utilis�e lorsqu'on veut utiliser une
  m�thode externe de transformation.
  @param ATransformMethod   M�thode de transformation du flux
  @param AInput             Flux entrant
  @param AOwnsInput         True lib�re le flux d'entr�e lors de la destruction
*}
constructor TTransformationStream.Create(
  ATransformMethod: TTransformStreamMethod; AInput: TStream;
  AOwnsInput: Boolean = False);
begin
  inherited Create;

  FTransformMethod := ATransformMethod;
  FInput := AInput;
  FOwnsInput := AOwnsInput;

  FPosition := 0;
end;

{*
  Cr�e un flux par transformation
  Cette variante de Create doit �tre utilis�e lorsque la m�thode
  TransformStream est surcharg�e, au lieu d'utiliser une m�thode externe de
  transformation.
  @param AInput       Flux entrant (d�faut = nil)
  @param AOwnsInput   True lib�re le flux d'entr�e lors de la destruction
*}
constructor TTransformationStream.Create(AInput: TStream = nil;
  AOwnsInput: Boolean = False);
begin
  Create(TTransformStreamMethod(nil), AInput, AOwnsInput);
end;

{*
  [@inheritDoc]
*}
destructor TTransformationStream.Destroy;
begin
  if FOwnsInput then
    FInput.Free;
  inherited;
end;

{*
  Augmente la variable interne de position dans le flux
  @param Value   Incr�ment de position
*}
procedure TTransformationStream.IncPosition(const Value: Int64);
begin
  Inc(FPosition, Value);
end;

{*
  Transforme le flux
*}
procedure TTransformationStream.TransformStream;
begin
  TransformMethod(Input, Self);
end;

{*
  [@inheritDoc]
*}
function TTransformationStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := FPosition;
  if (Offset <> 0) or (Origin <> soCurrent) then
    Error(@SInvalidStreamOp);
end;

{*
  D�clenche une erreur ETransformationStreamError
  @param Msg    Cha�ne de format du message
  @param Data   Param�tre du format
*}
class procedure TTransformationStream.Error(const Msg: string;
  Data: Integer = 0);

  // TODO FPC
  //function ReturnAddr: Pointer;
  //asm
  //      MOV     EAX,[EBP+4]
  //end;

begin
 // raise ETransformationStreamError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

{*
  D�clenche une erreur ETransformationStreamError
  @param Msg    Cha�ne de ressource de format du message
  @param Data   Param�tre du format
*}
class procedure TTransformationStream.Error(Msg: PResStringRec;
  Data: Integer = 0);
begin
  Error(LoadResString(Msg), Data);
end;

{--------------------------------------}
{ TCoroutineTransformationStream class }
{--------------------------------------}

{*
  Cr�e un flux par transformation
  Cette variante de Create doit �tre utilis�e lorsqu'on veut utiliser une
  m�thode externe de transformation.
  @param ATransformMethod   M�thode de transformation du flux
  @param AInput             Flux entrant
  @param AOwnsInput         True lib�re le flux d'entr�e lors de la destruction
*}
constructor TCoroutineTransformationStream.Create(
  ATransformMethod: TTransformStreamMethod; AInput: TStream;
  AOwnsInput: Boolean = False);
begin
  inherited Create(ATransformMethod, AInput, AOwnsInput);

  FCoroutine := TCoroutine.Create(PrivTransformStream, clNoLoop);

  FBuffer := nil;
  FBufferSize := 0;
end;

{*
  Cr�e un flux par transformation
  Cette variante de Create doit �tre utilis�e lorsque la m�thode
  TransformStream est surcharg�e, au lieu d'utiliser une m�thode externe de
  transformation.
  @param AInput       Flux entrant
  @param AOwnsInput   True lib�re le flux d'entr�e lors de la destruction
*}
constructor TCoroutineTransformationStream.Create(AInput: TStream = nil;
  AOwnsInput: Boolean = False);
begin
  Create(TTransformStreamMethod(nil), AInput, AOwnsInput);
end;

{*
  [@inheritDoc]
*}
destructor TCoroutineTransformationStream.Destroy;
begin
  FCoroutine.Free;
  inherited;
end;

{*
  M�thode d'ex�cution de la coroutine
  @param Coroutine   Coroutine
*}
procedure TCoroutineTransformationStream.PrivTransformStream(
  Coroutine: TCoroutine);
begin
  TransformStream;
end;

{*
  [@inheritDoc]
*}
function TCoroutineTransformationStream.Read(var Buffer;
  Count: Longint): Longint;
begin
  if Coroutine.CoroutineRunning then
    Error(@SInvalidStreamOp);

  if Coroutine.Terminated or (Count = 0) then
    Result := 0
  else
  begin
    FBuffer := @Buffer;
    FBufferSize := Count;
    Coroutine.Invoke;

    Result := Count - FBufferSize;
  end;
end;

{*
  [@inheritDoc]
*}
function TCoroutineTransformationStream.Write(const Buffer;
  Count: Longint): Longint;
var
  TempCount: Longint;
begin
  if not Coroutine.CoroutineRunning then
    Error(@SInvalidStreamOp);

  Result := 0;
  if Coroutine.Terminating then
    Exit;

  // Multiple Read vs 1 Write
  while Count-Result >= FBufferSize do
  begin
    Move(Pointer(PtrUInt(@Buffer) + Result)^, FBuffer^, FBufferSize);
    Inc(Result, FBufferSize);
    IncPosition(FBufferSize);
    FBuffer := nil;
    FBufferSize := 0;

    Coroutine.Yield;
    if Coroutine.Terminating then
      Exit;
  end;

  // Multiple Write vs 1 Read
  TempCount := Count-Result;
  if TempCount > 0 then
  begin
    Move(PPtrInt(PtrInt(@Buffer) + Result)^, FBuffer^, TempCount);
    Result := Count;
    IncPosition(TempCount);
    Inc(PtrInt(FBuffer), TempCount);
    Dec(FBufferSize, TempCount);
  end;
end;

{-----------------------------------}
{ TThreadTransformationStream class }
{-----------------------------------}

{*
  Cr�e un flux par transformation
  Cette variante de Create doit �tre utilis�e lorsqu'on veut utiliser une
  m�thode externe de transformation.
  @param ATransformMethod   M�thode de transformation du flux
  @param AInput             Flux entrant
  @param AOwnsInput         True lib�re le flux d'entr�e lors de la destruction
  @param ABufferSizeSize    Taille du buffer (d�faut = DefaultBufferSize)
*}
constructor TThreadTransformationStream.Create(
  ATransformMethod: TTransformStreamMethod; AInput: TStream;
  AOwnsInput: Boolean = False; ABufferSize: Integer = DefaultBufferSize);
begin
  if ABufferSize <= 0 then
    Error(@SBadBufferSize, ABufferSize);

  inherited Create(ATransformMethod, AInput, AOwnsInput);

  FThread := TMethodThread.Create(PrivTransformStream, True);
  FThread.OnDoTerminate := ThreadTerminated;

  {$IFDEF FPC}
  FWrittenSomethingEvent := TSimpleEvent.Create;
  FReadSomethingEvent := TSimpleEvent.Create;
  {$ELSE}
  FWrittenSomethingEvent := TEvent.Create;
  FReadSomethingEvent := TEvent.Create;
  {$ENDIF}

  FWritePosition := 0;
  FBufferSize := ABufferSize;
  GetMem(FBuffer, FBufferSize);
  FBufferCount := 0;
  FBufferWritePos := 0;
  FBufferReadPos := 0;
  FTerminated := False;
end;

{*
  Cr�e un flux par transformation
  Cette variante de Create doit �tre utilis�e lorsque la m�thode
  TransformStream est surcharg�e, au lieu d'utiliser une m�thode externe de
  transformation.
  @param AInput            Flux entrant
  @param AOwnsInput        True lib�re le flux d'entr�e lors de la destruction
  @param ABufferSizeSize   Taille du buffer (d�faut = DefaultBufferSize)
*}
constructor TThreadTransformationStream.Create(AInput: TStream = nil;
  AOwnsInput: Boolean = False; ABufferSize: Integer = DefaultBufferSize);
begin
  Create(TTransformStreamMethod(nil), AInput, AOwnsInput, ABufferSize);
end;

{*
  [@inheritDoc]
*}
destructor TThreadTransformationStream.Destroy;
begin
  FThread.Free;
  FWrittenSomethingEvent.Free;
  FReadSomethingEvent.Free;

  FreeMem(FBuffer);

  inherited;
end;

{*
  M�thode d'ex�cution du thread
  @param Thread   Thread
*}
procedure TThreadTransformationStream.PrivTransformStream(
  Thread: TMethodThread);
begin
  TransformStream;
end;

{*
  Gestionnaire de l'�v�nement OnTerminate du thread
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TThreadTransformationStream.ThreadTerminated(Sender: TObject);
begin
  FTerminated := True;
  WrittenSomethingEvent.SetEvent;
end;

{*
  Augmente la variable interne de position en �criture dans le flux
  @param Value   Incr�ment de position
*}
procedure TThreadTransformationStream.IncWritePosition(const Value: Int64);
begin
  Inc(FWritePosition, Value);
end;

{*
  Lit une partie du buffer interne
  InternalRead ne doit �tre appel� qu'avec une valeur de Count inf�rieure ou
  �gale � BufferCount.
  Tout appel � InternalRead doit �tre prot�g� d'une concurrence avec un appel �
  InternalWrite via la section critique CriticSec.
  @param Buffer   Adresse du buffer o� stocker les donn�es lues
  @param Count    Nombre d'octets � lire
*}
procedure TThreadTransformationStream.InternalRead(Dest: Pointer;
  Count: Integer);
var
  Count1, Count2: Integer;
begin
  if Count > BufferCount then
    Error(@SBufferIsntThatLarge, Count);

  // Count1 is before looping, Count2 is after looping
  Count1 := FBufferSize - FBufferReadPos;
  if Count1 > Count then
    Count1 := 0;
  Count2 := Count - Count1;

  // Before looping
  if Count1 > 0 then
  begin
    Move(PPtrInt(PtrInt(FBuffer) + FBufferReadPos)^, Dest^, Count1);
    Inc(PtrInt(Dest), Count1);
    FBufferReadPos := 0;
  end;

  // After looping
  Move(PPtrInt(PtrInt(FBuffer) + FBufferReadPos)^, Dest^, Count2);
  Inc(FBufferReadPos, Count2);

  // Update state
  IncPosition(Count);
  InterlockedAdd(FBufferCount, -Count);
end;

{*
  Ecrit des donn�es dans le buffer interne
  InternalWrite ne doit �tre appel� qu'avec une valeur de Count inf�rieure ou
  �gale � BufferSize-BufferCount.
  Tout appel � InternalWrite doit �tre prot�g� d'une concurrence avec un appel
  � InternalRead via la section critique CriticSec.
  @param Buffer   Adresse du buffer o� stocker les donn�es lues
  @param Count    Nombre d'octets � lire
*}
procedure TThreadTransformationStream.InternalWrite(Source: Pointer;
  Count: Integer);
var
  Count1, Count2: Integer;
begin
  if Count > BufferSize-BufferCount then
    Error(@SBufferIsntThatLarge, Count);

  // Count1 is before looping, Count2 is after looping
  Count1 := FBufferSize - FBufferWritePos;
  if Count1 > Count then
    Count1 := 0;
  Count2 := Count - Count1;

  // Before looping
  if Count1 > 0 then
  begin
    Move(Source^, PPtrInt(PtrInt(FBuffer) + FBufferWritePos)^, Count1);
    Inc(PtrInt(Source), Count1);
    FBufferWritePos := 0;
  end;

  // After looping
  Move(Source^, PPtrInt(PtrInt(FBuffer) + FBufferWritePos)^, Count2);
  Inc(FBufferWritePos, Count2);

  // Update state
  IncWritePosition(Count);
  InterlockedAdd(FBufferCount, Count);
end;

{*
  D�clenche dans le contexte courant l'exception fatale du thread
  En r�alit�, RaiseFatalException cr�e une copie grossi�re de l'exception
  g�n�r�e au d�part. En effet, TThread ne permet pas d'indiquer qu'il ne faut
  plus lib�rer FatalException, et l'on ne peut donc pas d�clencher cette
  exception exactement.
*}
procedure TThreadTransformationStream.RaiseFatalException;
begin
  with Thread do
  begin
    if FatalException is Exception then
      raise ExceptClass(FatalException.ClassType).Create(
        Exception(FatalException).Message)
    else
      raise TClass(FatalException.ClassType).Create;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TThreadTransformationStream.AfterConstruction;
begin
  inherited;
  FThread.Suspended := False;
end;

{*
  [@inheritDoc]
*}
procedure TThreadTransformationStream.BeforeDestruction;
begin
  inherited;
  Thread.Terminate;
  Thread.WaitFor;
end;

{*
  [@inheritDoc]
*}
function TThreadTransformationStream.Read(var Buffer;
  Count: Longint): Longint;
var
  BufCount, SubCount: Longint;
begin
  Result := 0;

  while Count > 0 do
  begin
    if Assigned(Thread.FatalException) then
      RaiseFatalException;

    BufCount := BufferCount;
    if Terminated and (BufCount = 0) then
      Exit;

    // Actual reading
    if Count < BufCount then
      SubCount := Count
    else
      SubCount := BufCount;

    InternalRead(PPtrInt(PtrInt(@Buffer) + Result), SubCount);

    if not Terminated then
      WrittenSomethingEvent.ResetEvent;

    // Update Result and Count
    Inc(Result, SubCount);
    Dec(Count, SubCount);

    // Synchronization jobs
    ReadSomethingEvent.SetEvent;
    if Count > 0 then
      WrittenSomethingEvent.WaitFor(INFINITE);
  end;
end;

{*
  [@inheritDoc]
*}
function TThreadTransformationStream.Write(const Buffer;
  Count: Longint): Longint;
var
  BufCount, SubCount: Longint;
begin
  Result := 0;

  while Count > 0 do
  begin
    if Thread.Terminated then
      Exit;

    // Actual writing
    BufCount := BufferCount;
    if Count < BufferSize-BufCount then
      SubCount := Count
    else
      SubCount := BufferSize-BufCount;

    InternalWrite(PPtrInt(PtrInt(@Buffer) + Result), SubCount);

    ReadSomethingEvent.ResetEvent;

    // Update Result and Count
    Inc(Result, SubCount);
    Dec(Count, SubCount);

    // Synchronization jobs
    WrittenSomethingEvent.SetEvent;
    if Count > 0 then
      ReadSomethingEvent.WaitFor(INFINITE);
  end;
end;

{*
  [@inheritDoc]
*}
function TThreadTransformationStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) and
    (GetCurrentThreadID = Thread.ThreadID) then
    Result := FWritePosition
  else
    Result := inherited Seek(Offset, Origin);
end;

end.

