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
  Classes de gestion de coroutines
  ScCoroutines propose deux classes principales. TCustomCoroutine est une
  classe abstraite g�rant des coroutines. Il faut la sucharger � la mani�re
  dont on surcharge TThread pour avoir une coroutine concr�te. TCoroutine est
  une impl�mentation de celle-ci qui prend une m�thode en param�tre, et ex�cute
  celle-ci comme coroutine.
  @author sjrd
  @version 1.0
*}
unit ScCoroutines;
{$i ..\..\source\Sepi.inc}
interface

uses
  Windows, SysUtils, Classes, SyncObjs, ScClasses;

resourcestring
  SCoroutInternalError =
    'Erreur interne de la coroutine';
  SCoroutInvalidOpWhileRunning =
    'Op�ration invalide lorsque la coroutine est en ex�cution';
  SCoroutInvalidOpWhileNotRunning =
    'Op�ration invalide lorsque la coroutine n''est pas en ex�cution';
  SCoroutTerminating =
    'La coroutine est en train de se terminer';
  SCoroutTerminated =
    'Impossible de continuer : la coroutine est termin�e';
  SCoroutNotTerminated =
    'Impossible de r�initialiser : la coroutine n''est pas termin�e';

type
  TCoroutine = class;

  {*
    Type de boucle de coroutine
    - clNoLoop : ex�cut�e une fois, ne boucle pas
    - clImmediate : relance imm�diatement jusqu'au premier Yield
    - clNextInvoke : relance lors du prochain appel � Invoke
  *}
  TCoroutineLoop = (clNoLoop, clImmediate, clNextInvoke);

  /// Erreur li�e � l'utilisation d'une coroutine
  ECoroutineError = class(Exception);

  /// Interruption pr�matur�e d'une coroutine
  ECoroutineTerminating = class(Exception);

  /// M�thode de contenu d'une coroutine publique
  TCoroutineMethod = procedure(Coroutine: TCoroutine) of object;

  {*
    Classe de support des coroutines
    La m�thode Invoke ne peut avoir qu'une seule ex�cution � la fois. Elle ne
    peut ni �tre appel�e dans deux threads diff�rents en m�me temps ; ni �tre
    appel�e depuis Execute (ce qui constitue un appel r�cursif).
    En revanche, elle peut �tre appel�e successivement par deux threads
    diff�rents.

    La propri�t� Loop d�termine le comportement de bouclage de la coroutine.
    Celle-ci peut soit ne pas boucler (clNoLoop) : un appel � Invoke lorsque
    Terminated vaut True d�clenchera une exception. Soit boucler imm�diatement
    (clImmediate) : d�s que Execute se termine, elle est rappel�e sans revenir
    � l'appelant. Soit boucler au prochain Invoke : dans ce cas l'appelant
    reprend la main entre la fin d'une ex�cution et le d�but de la suivante.

    La proc�dure Execute devrait tester l'�tat de Terminating apr�s chaque
    appel � Yield, et se terminer proprement si cette propri�t� vaut True.
    Cette propri�t� sera positionn�e � True lorsque l'objet coroutine devra se
    lib�rer, avant de relancer l'ex�cution. Si un appel � Yield est fait dans
    cet �tat, une exception de type ECoroutineTerminating assure que celle-ci
    se termine imm�diatement.

    @author sjrd
    @version 1.0
  *}
  TCustomCoroutine = class(TObject)
  private
    FLoop: TCoroutineLoop; /// Type de boucle de la coroutine

    FCoroutineRunning: Boolean; /// True si la coroutine est cours d'ex�cution
    FTerminating: Boolean;      /// True si la coroutine doit se terminer
    FTerminated: Boolean;       /// True si la coroutine est termin�e

    FCoroutineThread: TMethodThread; /// Thread qui ex�cute la coroutine
    FInvokeEvent: TEvent;            /// �v�nement d�clench� sur Invoke
    FYieldEvent: TEvent;             /// �v�nement d�clench� sur Yield

    FExceptObject: TObject;  /// Objet exception d�clench�e par la coroutine
    FExceptAddress: Pointer; /// Adresse de d�clenchement de l'exception

    procedure WaitForNoResult(Event: TEvent);
    procedure InitCoroutine;
    procedure Main(Thread: TMethodThread);
    procedure EnterCoroutine;
    procedure LeaveCoroutine;
    procedure Terminate(Sender: TObject);
  protected
    procedure Invoke;
    procedure Yield;
    procedure Reset;

    {*
      Coroutine � ex�cuter
      Surchargez Execute pour donner le code de la coroutine.
    *}
    procedure Execute; virtual; abstract;

    property Loop: TCoroutineLoop read FLoop write FLoop;

    property CoroutineRunning: Boolean read FCoroutineRunning;
    property Terminating: Boolean read FTerminating;
    property Terminated: Boolean read FTerminated;
  public
    constructor Create(ALoop: TCoroutineLoop = clNoLoop);
    destructor Destroy; override;

    procedure BeforeDestruction; override;

    class procedure Error(const Msg: string;
      Data: Integer = 0); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer = 0); overload;
  end;

  {*
    Impl�mentation publique de TCustomCoroutine
    @author sjrd
    @version 1.0
  *}
  TCoroutine = class(TCustomCoroutine)
  private
    FExecuteMethod: TCoroutineMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(AExecuteMethod: TCoroutineMethod;
      ALoop: TCoroutineLoop = clNoLoop);

    procedure Invoke;
    procedure Yield;
    procedure Reset;

    property Loop;
    property CoroutineRunning;
    property Terminating;
    property Terminated;
  end;

  {*
    �num�rateur en coroutine
    Pour obtenir un �num�rateur concret, il faut surcharger la m�thode Execute
    pour d�finir le code de l'�num�rateur. Et red�finir une m�thode Yield
    acceptant un param�tre du type des valeurs � �num�rer. Cette m�thode doit
    stocker le param�tre de mani�re � pouvoir y acc�der via une propri�t�
    Current, puis appeler la m�thode Yield h�rit�e de TCoroutine.
    @author sjrd, sur une id�e de Sergey Antonov
    @version 1.0
  *}
  TCustomEnumerator = class(TCustomCoroutine)
  public
    function MoveNext: Boolean;
  end;

implementation

{------------------------}
{ TCustomCoroutine class }
{------------------------}

{*
  Cr�e une coroutine avec une taille de pile donn�e
  @param ALoop       Type de boucle de la coroutine (d�faut : clNoLoop)
  @param StackSize   Taille de la pile (d�faut : DefaultStackSize)
*}
constructor TCustomCoroutine.Create(ALoop: TCoroutineLoop = clNoLoop);
begin
  inherited Create;

  // Set up configuration
  FLoop := ALoop;

  // Set up original state
  FCoroutineRunning := False;
  FTerminating := False;
  FTerminated := False;

  // Create thread and events
  FInvokeEvent := TEvent.Create(nil, False, False, '');
  FYieldEvent := TEvent.Create(nil, False, False, '');

  // Initialize coroutine
  InitCoroutine;
end;

{*
  D�truit l'instance
*}
destructor TCustomCoroutine.Destroy;
begin
  // Release thread and events
  FCoroutineThread.Free;
  FYieldEvent.Free;
  FInvokeEvent.Free;

  inherited;
end;

{*
  Attend un �v�nement, sans r�sultat
  En cas d'erreur, d�clenche une exception au lieu de renvoyer un r�sultat.
  @param Event   �v�nement � attendre
  @throws ECoroutineError Erreur lors de l'attente
*}
procedure TCustomCoroutine.WaitForNoResult(Event: TEvent);
begin
  if Event.WaitFor(INFINITE) <> wrSignaled then
    Error(@SCoroutInternalError);
end;

{*
  Initialise la coroutine avant sa premi�re ex�cution
*}
procedure TCustomCoroutine.InitCoroutine;
begin
  FreeAndNil(FCoroutineThread);
  FExceptObject := nil;

  FInvokeEvent.ResetEvent;
  FYieldEvent.ResetEvent;

  FCoroutineThread := TMethodThread.Create(Main);
  FCoroutineThread.OnDoTerminate := Terminate;
end;

{*
  M�thode principale de la coroutine
*}
procedure TCustomCoroutine.Main(Thread: TMethodThread);
begin
  try
    WaitForNoResult(FInvokeEvent);

    if not Terminating then
    repeat
      Execute;
      if (Loop = clNextInvoke) and (not Terminating) then
        Yield;
    until (Loop = clNoLoop) or Terminating;
  except
    FExceptObject := AcquireExceptionObject;
    FExceptAddress := ExceptAddr;
  end;
end;

{*
  Entre dans la coroutine
*}
procedure TCustomCoroutine.EnterCoroutine;
begin
  FCoroutineRunning := True;
  FInvokeEvent.SetEvent;
  WaitForNoResult(FYieldEvent);
end;

{*
  Sort la coroutine
*}
procedure TCustomCoroutine.LeaveCoroutine;
begin
  FCoroutineRunning := False;
  FYieldEvent.SetEvent;
  WaitForNoResult(FInvokeEvent);
end;

{*
  Termine la coroutine
*}
procedure TCustomCoroutine.Terminate(Sender: TObject);
begin
  FTerminated := True;
  FCoroutineRunning := False;
  FYieldEvent.SetEvent;
end;

{*
  Ex�cute la coroutine jusqu'au prochain appel � Yield
*}
procedure TCustomCoroutine.Invoke;
var
  TempError: TObject;
begin
  if CoroutineRunning then
    Error(@SCoroutInvalidOpWhileRunning);
  if Terminated then
    Error(@SCoroutTerminated);

  EnterCoroutine;

  if Assigned(FExceptObject) then
  begin
    // Re-raise exception in the caller thread
    TempError := FExceptObject;
    FExceptObject := nil;
    if Loop <> clNoLoop then
      Reset;
    raise TempError at FExceptAddress;
  end;
end;

{*
  Rend la main � l'appelant - retournera lors du prochain appel � Invoke
*}
procedure TCustomCoroutine.Yield;
begin
  if not CoroutineRunning then
    Error(@SCoroutInvalidOpWhileNotRunning);
  if Terminating then
    raise ECoroutineTerminating.CreateRes(@SCoroutTerminating);

  LeaveCoroutine;
end;

{*
  R�initialise compl�tement la coroutine
  La coroutine doit �tre termin�e pour appeler Reset (Terminated = True).
  Reset peut �galement �tre appel�e si la coroutine s'est termin�e � cause
  d'une exception.
*}
procedure TCustomCoroutine.Reset;
begin
  if CoroutineRunning then
    Error(@SCoroutInvalidOpWhileRunning);
  if not Terminated then
    Error(@SCoroutNotTerminated);

  FTerminated := False;
  InitCoroutine;
end;

{*
  Appel� juste avant le premier destructeur
  BeforeDestruction assure qu'on n'essaie pas de d�truire l'objet coroutine
  depuis le code de la coroutine.
  Si la coroutine n'a pas termin� son ex�cution lors du dernier appel � Invoke,
  BeforeDestruction tente de la faire se terminer correctement. Si un appel �
  Yield survient, une exception ECoroutineTerminating est d�clench�e pour
  forcer la coroutine � se terminer.
*}
procedure TCustomCoroutine.BeforeDestruction;
begin
  if FCoroutineRunning then
    Error(@SCoroutInvalidOpWhileRunning);

  FTerminating := True;

  if not Terminated then
  begin
    EnterCoroutine;
    if Assigned(FExceptObject) then
      FExceptObject.Free;
  end;

  inherited;
end;

{*
  D�clenche une erreur ECoroutineError
  @param Msg    Cha�ne de format du message
  @param Data   Param�tre du format
*}
class procedure TCustomCoroutine.Error(const Msg: string; Data: Integer = 0);

  function ReturnAddr: Pointer;
  asm
        MOV     EAX,[EBP+4]
  end;

begin
  raise ECoroutineError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

{*
  D�clenche une erreur ECoroutineError
  @param Msg    Cha�ne de ressource de format du message
  @param Data   Param�tre du format
*}
class procedure TCustomCoroutine.Error(Msg: PResStringRec; Data: Integer = 0);
begin
  Error(LoadResString(Msg), Data);
end;

{------------------}
{ TCoroutine class }
{------------------}

{*
  Cr�e une coroutine
  @param AExecuteMethod   M�thode contenu de la coroutine
  @param ALoop            Type de boucle de la coroutine (d�faut : clNoLoop)
*}
constructor TCoroutine.Create(AExecuteMethod: TCoroutineMethod;
  ALoop: TCoroutineLoop = clNoLoop);
begin
  inherited Create(ALoop);
  FExecuteMethod := AExecuteMethod;
end;

{*
  [@inheritDoc]
*}
procedure TCoroutine.Execute;
begin
  FExecuteMethod(Self);
end;

{*
  [@inheritDoc]
*}
procedure TCoroutine.Invoke;
begin
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TCoroutine.Yield;
begin
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TCoroutine.Reset;
begin
  inherited;
end;

{-------------------------}
{ TCustomEnumerator class }
{-------------------------}

{*
  Passe � l'�l�ment suivant de l'�num�rateur
  @return True s'il y a encore un �l�ment, False si l'�num�rateur est termin�
*}
function TCustomEnumerator.MoveNext: Boolean;
begin
  if not Terminated then
    Invoke;
  Result := not Terminated;
end;

end.

