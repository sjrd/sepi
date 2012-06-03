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
  Extension de Classes
  @author sjrd
  @version 1.0
*}
unit ScClasses;
{$i ..\..\source\Sepi.inc}
interface

uses
  Classes;

type
  TMethodThread = class;

  {*
    Flux travaillant dans la mémoire vive de façon absolue
    @author sjrd
    @version 1.0
  *}
  TAbsoluteMemoryStream = class(TStream)
  private
    FPosition: Pointer;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(APosition: Pointer = nil);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    property PointerPos: Pointer read FPosition write FPosition;
  end;

  {*
    Flux virtuel qui ne lit/enregistre rien, mais retient la taille requise
    TMeasureStream peut être utilisé pour mesurer a priori la taille requise
    par une méthode qui écrit dans un flux.
    @author sjrd
    @version 1.0
  *}
  TMeasureStream = class(TStream)
  private
    FSize: Int64;     /// Taille
    FPosition: Int64; /// Position
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  {*
    Prototype d'une méthode d'exécution d'un thread
    @param Thread   Objet thread qui contrôle le thread de la méthode
  *}
  TThreadedMethod = procedure(Thread: TMethodThread) of object;

  {*
    Thread dont on fournit la méthode d'exécution en externe
    @author sjrd
    @version 1.0
  *}
  TMethodThread = class(TThread)
  private
    FExecuteMethod: TThreadedMethod; /// Méthode d'exécution du thread
    /// Appelé dans le contexte du thread lorsque celui-ci se termine
    FOnDoTerminate: TNotifyEvent;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(AExecuteMethod: TThreadedMethod;
      CreateSuspended: Boolean = False);

    property ReturnValue;
    property Terminated;
    property OnDoTerminate: TNotifyEvent
      read FOnDoTerminate write FOnDoTerminate;
  end;

implementation

{---------------------------- }
{ TAbsoluteMemoryStream class }
{-----------------------------}

{*
  Crée un nouveau flux de mémoire vive
  @param APosition   Adresse initiale
*}
constructor TAbsoluteMemoryStream.Create(APosition: Pointer = nil);
begin
  inherited Create;
  FPosition := APosition;
end;

{*
  [@inheritDoc]
*}
function TAbsoluteMemoryStream.GetSize: Int64;
begin
  Result := Int64($100000000);
end;

{*
  [@inheritDoc]
*}
function TAbsoluteMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  Move(FPosition^, Buffer, Count);
  Inc(Integer(FPosition), Count);
  Result := Count;
end;

{*
  [@inheritDoc]
*}
function TAbsoluteMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  Move(Buffer, FPosition^, Count);
  Inc(Integer(FPosition), Count);
  Result := Count;
end;

{*
  [@inheritDoc]
*}
function TAbsoluteMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning, soFromEnd: FPosition := Pointer(Offset);
    soFromCurrent: Inc(Integer(FPosition), Offset);
  end;
  Result := Longint(FPosition);
end;

{----------------------}
{ TMeasureStream class }
{----------------------}

{*
  [@inheritDoc]
*}
function TMeasureStream.GetSize: Int64;
begin
  Result := FSize;
end;

{*
  [@inheritDoc]
*}
procedure TMeasureStream.SetSize(const NewSize: Int64);
begin
  FSize := NewSize;
end;

{*
  [@inheritDoc]
*}
function TMeasureStream.Read(var Buffer; Count: Integer): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;

    if Result > 0 then
    begin
      if Result > Count then
        Result := Count;
      Inc(FPosition, Result);
    end else
      Result := 0;
  end else
    Result := 0;
end;

{*
  [@inheritDoc]
*}
function TMeasureStream.Write(const Buffer; Count: Integer): Longint;
var
  Pos: Int64;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
        FSize := Pos;

      FPosition := Pos;
      Result := Count;
    end else
      Result := 0;
  end else
    Result := 0;
end;

{*
  [@inheritDoc]
*}
function TMeasureStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize + Offset;
  end;

  Result := FPosition;
end;

{---------------------}
{ TMethodThread class }
{---------------------}

{*
  Crée l'objet thread
  @param AExecuteMethod    Méthode d'exécution du thread
  @param CreateSuspended   True commence en état suspendu (défaut = False)
*}
constructor TMethodThread.Create(AExecuteMethod: TThreadedMethod;
  CreateSuspended: Boolean = False);
begin
  FExecuteMethod := AExecuteMethod;
  inherited Create(CreateSuspended);
end;

{*
  [@inheritDoc]
*}
procedure TMethodThread.Execute;
begin
  FExecuteMethod(Self);
end;

{*
  [@inheritDoc]
*}
procedure TMethodThread.DoTerminate;
begin
  if Assigned(OnDoTerminate) then
    OnDoTerminate(Self);
  inherited;
end;

end.

