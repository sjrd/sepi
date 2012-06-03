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
  Classes de gestion de coroutines
  ScCoroutines propose deux classes principales. TCustomCoroutine est une
  classe abstraite gérant des coroutines. Il faut la sucharger à la manière
  dont on surcharge TThread pour avoir une coroutine concrète. TCoroutine est
  une implémentation de celle-ci qui prend une méthode en paramètre, et exécute
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
    'Opération invalide lorsque la coroutine est en exécution';
  SCoroutInvalidOpWhileNotRunning =
    'Opération invalide lorsque la coroutine n''est pas en exécution';
  SCoroutTerminating =
    'La coroutine est en train de se terminer';
  SCoroutTerminated =
    'Impossible de continuer : la coroutine est terminée';
  SCoroutNotTerminated =
    'Impossible de réinitialiser : la coroutine n''est pas terminée';

type
  TCoroutine = class;

  {*
    Type de boucle de coroutine
    - clNoLoop : exécutée une fois, ne boucle pas
    - clImmediate : relance immédiatement jusqu'au premier Yield
    - clNextInvoke : relance lors du prochain appel à Invoke
  *}
  TCoroutineLoop = (clNoLoop, clImmediate, clNextInvoke);

  /// Erreur liée à l'utilisation d'une coroutine
  ECoroutineError = class(Exception);

  /// Interruption prématurée d'une coroutine
  ECoroutineTerminating = class(Exception);

  /// Méthode de contenu d'une coroutine publique
  TCoroutineMethod = procedure(Coroutine: TCoroutine) of object;

  {*
    Classe de support des coroutines
    La méthode Invoke ne peut avoir qu'une seule exécution à la fois. Elle ne
    peut ni être appelée dans deux threads différents en même temps ; ni être
    appelée depuis Execute (ce qui constitue un appel récursif).
    En revanche, elle peut être appelée successivement par deux threads
    différents.

    La propriété Loop détermine le comportement de bouclage de la coroutine.
    Celle-ci peut soit ne pas boucler (clNoLoop) : un appel à Invoke lorsque
    Terminated vaut True déclenchera une exception. Soit boucler immédiatement
    (clImmediate) : dès que Execute se termine, elle est rappelée sans revenir
    à l'appelant. Soit boucler au prochain Invoke : dans ce cas l'appelant
    reprend la main entre la fin d'une exécution et le début de la suivante.

    La procédure Execute devrait tester l'état de Terminating après chaque
    appel à Yield, et se terminer proprement si cette propriété vaut True.
    Cette propriété sera positionnée à True lorsque l'objet coroutine devra se
    libérer, avant de relancer l'exécution. Si un appel à Yield est fait dans
    cet état, une exception de type ECoroutineTerminating assure que celle-ci
    se termine immédiatement.

    @author sjrd
    @version 1.0
  *}
  TCustomCoroutine = class(TObject)
  private
    FLoop: TCoroutineLoop; /// Type de boucle de la coroutine

    FCoroutineRunning: Boolean; /// True si la coroutine est cours d'exécution
    FTerminating: Boolean;      /// True si la coroutine doit se terminer
    FTerminated: Boolean;       /// True si la coroutine est terminée

    FCoroutineThread: TMethodThread; /// Thread qui exécute la coroutine
    FInvokeEvent: TEvent;            /// Événement déclenché sur Invoke
    FYieldEvent: TEvent;             /// Événement déclenché sur Yield

    FExceptObject: TObject;  /// Objet exception déclenchée par la coroutine
    FExceptAddress: Pointer; /// Adresse de déclenchement de l'exception

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
      Coroutine à exécuter
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
    Implémentation publique de TCustomCoroutine
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
    Énumérateur en coroutine
    Pour obtenir un énumérateur concret, il faut surcharger la méthode Execute
    pour définir le code de l'énumérateur. Et redéfinir une méthode Yield
    acceptant un paramètre du type des valeurs à énumérer. Cette méthode doit
    stocker le paramètre de manière à pouvoir y accéder via une propriété
    Current, puis appeler la méthode Yield héritée de TCoroutine.
    @author sjrd, sur une idée de Sergey Antonov
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
  Crée une coroutine avec une taille de pile donnée
  @param ALoop       Type de boucle de la coroutine (défaut : clNoLoop)
  @param StackSize   Taille de la pile (défaut : DefaultStackSize)
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
  Détruit l'instance
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
  Attend un événement, sans résultat
  En cas d'erreur, déclenche une exception au lieu de renvoyer un résultat.
  @param Event   Événement à attendre
  @throws ECoroutineError Erreur lors de l'attente
*}
procedure TCustomCoroutine.WaitForNoResult(Event: TEvent);
begin
  if Event.WaitFor(INFINITE) <> wrSignaled then
    Error(@SCoroutInternalError);
end;

{*
  Initialise la coroutine avant sa première exécution
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
  Méthode principale de la coroutine
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
  Exécute la coroutine jusqu'au prochain appel à Yield
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
  Rend la main à l'appelant - retournera lors du prochain appel à Invoke
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
  Réinitialise complètement la coroutine
  La coroutine doit être terminée pour appeler Reset (Terminated = True).
  Reset peut également être appelée si la coroutine s'est terminée à cause
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
  Appelé juste avant le premier destructeur
  BeforeDestruction assure qu'on n'essaie pas de détruire l'objet coroutine
  depuis le code de la coroutine.
  Si la coroutine n'a pas terminé son exécution lors du dernier appel à Invoke,
  BeforeDestruction tente de la faire se terminer correctement. Si un appel à
  Yield survient, une exception ECoroutineTerminating est déclenchée pour
  forcer la coroutine à se terminer.
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
  Déclenche une erreur ECoroutineError
  @param Msg    Chaîne de format du message
  @param Data   Paramètre du format
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
  Déclenche une erreur ECoroutineError
  @param Msg    Chaîne de ressource de format du message
  @param Data   Paramètre du format
*}
class procedure TCustomCoroutine.Error(Msg: PResStringRec; Data: Integer = 0);
begin
  Error(LoadResString(Msg), Data);
end;

{------------------}
{ TCoroutine class }
{------------------}

{*
  Crée une coroutine
  @param AExecuteMethod   Méthode contenu de la coroutine
  @param ALoop            Type de boucle de la coroutine (défaut : clNoLoop)
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
  Passe à l'élément suivant de l'énumérateur
  @return True s'il y a encore un élément, False si l'énumérateur est terminé
*}
function TCustomEnumerator.MoveNext: Boolean;
begin
  if not Terminated then
    Invoke;
  Result := not Terminated;
end;

end.

