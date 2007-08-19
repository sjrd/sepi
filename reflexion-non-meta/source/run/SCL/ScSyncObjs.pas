{*
  D�finit des classes de synchronisations entre threads
  @author sjrd
  @version 1.0
*}
unit ScSyncObjs;

interface

uses
  Classes, Contnrs, SyncObjs, ScLists;

type
  TScCustomTaskQueue = class;

  {*
    �tat d'une t�che
    - tsWaiting : en attente ;
    - tsProcessing : en cours ;
    - tsFinished : termin�e ;
    - tsCanceled : annul�e.
  *}
  TScTaskState = (tsWaiting, tsProcessing, tsFinished, tsCancelled);

  {*
    T�che g�r�e par une instance de TScTaskQueue
    @author sjrd
    @version 1.0
  *}
  TScTask = class
  private
    FOwner : TScCustomTaskQueue; /// File propri�taire de la t�che
    FState : TScTaskState;       /// �tat de la t�che
    FFreeOnFinished : boolean;   /// Se lib�re automatiquement

    FWaitForCount : integer; /// Nombre d'ex�cutions de WaitFor en cours

    FFatalException : TObject; /// Exception non intercept�e

    procedure Finish(AFatalException : TObject = nil);
  protected
    {*
      Ex�cute l'action
    *}
    procedure Execute; virtual; abstract;

    function Cancel(ForceFree : boolean = False) : boolean;

    property Owner : TScCustomTaskQueue read FOwner;
    property State : TScTaskState read FState;
    property FreeOnFinished : boolean
      read FFreeOnFinished write FFreeOnFinished;

    property FatalException : TObject read FFatalException;
  public
    constructor Create(AOwner : TScCustomTaskQueue;
      AFreeOnFinished : boolean); overload;
    constructor Create(AOwner : TScCustomTaskQueue); overload;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure WaitFor;
    procedure RaiseException;
  end;

  {*
    Gestionnaire de file (FIFO) de t�ches
    TScCustomTaskQueue est une classe de base pour l'impl�mentation de files de
    t�ches, qui s'ex�cutent de fa�on synchrone entre elles, mais asynchrone par
    rapport au contr�leur.
    Puisque TScCustomTaskQueue h�rite de TThread au lieu de l'encapsuler, le
    contr�leur a plein acc�s sur le d�roulement du thread de r�partition.
    Un appel � la m�thode Terminate mettra fin � la r�partition des t�ches,
    m�me si la file n'est pas vide.
    Pour attendre que la liste des messages en attente soit vide, utilisez la
    propri�t� Ready, et non la m�thode WaitFor. Pour attendre qu'une t�che en
    particulier soit termin�e, les descendants de TScCustomTaskQueue peuvent
    offrir l'acc�s aux objets TScTask, qui proposent une m�thode WaitFor.
    Vous pouvez positionner la propri�t� TerminateOnException � False si vous
    ne voulez pas que le traitement des messages s'arr�te en cas d'exception
    lors de l'un d'eux.
    Les descendants de TScTaskQueue ne devraient normalement pas r�impl�menter
    la m�thode Execute.
    @author sjrd
    @version 1.0
  *}
  TScCustomTaskQueue = class(TThread)
  private
    FWaitingQueue : TScWaitingObjectQueue; /// File des messages en attente
    FCriticalSection : TCriticalSection;   /// Section critique sur la liste

    /// Valeur par d�faut de la propri�t� FreeOnFinished des t�ches de la file
    FDefaultFreeOnFinished : boolean;

    /// Indique si le thread d'ex�cution doit se terminer en cas d'exception
    FTerminateOnException : boolean;

    FReady : boolean; /// Indique si la file a termin� son travail

    procedure Push(Task : TScTask);
    procedure Cancel(Task : TScTask);
    function Pop : TScTask;
  protected
    procedure Execute; override;

    property WaitingQueue : TScWaitingObjectQueue read FWaitingQueue;
    property CriticalSection : TCriticalSection read FCriticalSection;

    property DefaultFreeOnFinished : boolean
      read FDefaultFreeOnFinished write FDefaultFreeOnFinished;
    property TerminateOnException : boolean
      read FTerminateOnException write FTerminateOnException;

    property Ready : boolean read FReady;
  public
    constructor Create(ADefaultFreeOnFinished : boolean = True;
      ATerminateOnException : boolean = True);
    destructor Destroy; override;

    procedure BeforeDestruction; override;
  end;

  {*
    Gestionnaire de file (FIFO) de t�ches
    TScTaskQueue propose une impl�mentation enti�rement publique de
    TScCustomTaskQueue.
    @author sjrd
    @version 1.0
  *}
  TScTaskQueue = class(TScCustomTaskQueue)
  public
    property DefaultFreeOnFinished;
    property TerminateOnException;
    property Ready;
  end;

  {*
    T�che de r�partition de message
    TScCustomMessageTask fournit une impl�mentation prot�g�e d'une t�che de
    dispatching de message.
    @author sjrd
    @version 1.0
  *}
  TScCustomMessageTask = class(TScTask)
  private
    FMsg : Pointer;     /// Pointeur sur le message
    FMsgSize : integer; /// Taille du message
    FDestObj : TObject; /// Objet auquel d�livrer le message
  protected
    procedure Execute; override;

    procedure GetMsg(out Msg);

    property Msg : Pointer read FMsg;
    property MsgSize : integer read FMsgSize;
    property DestObj : TObject read FDestObj;
  public
    constructor Create(AOwner : TScCustomTaskQueue; const AMsg;
      AMsgSize : integer; ADestObj : TObject;
      AFreeOnFinished : boolean); overload;
    constructor Create(AOwner : TScCustomTaskQueue; const AMsg;
      AMsgSize : integer; ADestObj : TObject); overload;
    destructor Destroy; override;
  end;

  {*
    T�che de r�partition de message
    TScMessageTask est une version publique de TScCustomMessageTask.
    @author sjrd
    @version 1.0
  *}
  TScMessageTask = class(TScCustomMessageTask)
  public
    function Cancel(ForceFree : boolean = False) : boolean;
    procedure GetMsg(out Msg);

    property Owner;
    property State;
    property FreeOnFinished;

    property Msg;
    property DestObj;

    property FatalException;
  end;

function InterlockedIncrement(var I : integer) : integer;
function InterlockedDecrement(var I : integer) : integer;

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  Incr�mente un compteur de fa�on thread-safe
  @param Compteur � incr�menter
  @return Nouvelle valeur du compteur
*}
function InterlockedIncrement(var I : integer) : integer;
asm
        MOV     EDX,1
   LOCK XADD    [EAX],EDX
        LEA     EAX,[EDX+1]
end;

{*
  D�cr�mente un compteur de fa�on thread-safe
  @param Compteur � d�cr�menter
  @return Nouvelle valeur du compteur
*}
function InterlockedDecrement(var I : integer) : integer;
asm
        MOV     EDX,-1
   LOCK XADD    [EAX],EDX
        LEA     EAX,[EDX-1]
end;

{---------------}
{ TScTask class }
{---------------}

{*
  Cr�e une t�che en sp�cifiant FreeOnFinished
  @param AOwner            File de t�ches propri�taire
  @param AFreeOnFinished   Indique si doit se d�truire automatiquement
*}
constructor TScTask.Create(AOwner : TScCustomTaskQueue;
  AFreeOnFinished : boolean);
begin
  inherited Create;

  FOwner := AOwner;
  FState := tsWaiting;
  FFreeOnFinished := AFreeOnFinished;
  FWaitForCount := 0;
  FFatalException := nil;
end;

{*
  Cr�e une t�che
  @param AOwner   File de t�ches propri�taire
*}
constructor TScTask.Create(AOwner : TScCustomTaskQueue);
begin
  Create(AOwner, AOwner.DefaultFreeOnFinished);
end;

{*
  [@inheritDoc]
*}
destructor TScTask.Destroy;
begin
  FFatalException.Free;
  inherited;
end;

{*
  Termine la t�che
  @param AFatalException   Exception non intercept�e
*}
procedure TScTask.Finish(AFatalException : TObject = nil);
begin
  Assert(State in [tsWaiting, tsProcessing]);
  FFatalException := AFatalException;

  if State = tsProcessing then
    FState := tsFinished
  else
    FState := tsCancelled;

  if FreeOnFinished then
    Free;
end;

{*
  Annule la t�che
  Cancel ne fait rien si la t�che est d�j� en cours ou termin�e
  @param ForceFree   � True, force la lib�ration, m�me si FreeOnFinished = False
  @return True si r�ussi, False si la t�che �tait d�j� en cours ou termin�e
*}
function TScTask.Cancel(ForceFree : boolean = False) : boolean;
begin
  Owner.CriticalSection.Acquire;
  try
    Result := State = tsWaiting;
    if Result then
    begin
      if ForceFree then
        FFreeOnFinished := True;
      Owner.Cancel(Self);
    end;
  finally
    Owner.CriticalSection.Release;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TScTask.AfterConstruction;
begin
  inherited;
  Owner.Push(Self);
end;

{*
  [@inheritDoc]
*}
procedure TScTask.BeforeDestruction;
begin
  inherited;

  while FWaitForCount > 0 do;
end;

{*
  Attend que la t�che soit termin�e
  WaitFor peut �tre appel�e sans risque sur une t�che dont la propri�t�
  FreeOnFinished vaut True.
*}
procedure TScTask.WaitFor;
begin
  InterlockedIncrement(FWaitForCount);
  try
    while State in [tsWaiting, tsProcessing] do;
  finally
    InterlockedDecrement(FWaitForCount);
  end;
end;

{*
  D�clenche l'exception FatalException, si celle-ci est non nulle
  Utilisez RaiseException plut�t que "raise FatalException", ou vous courrez �
  la violation d'acc�s lors de la lib�ration de l'objet TScTask.
*}
procedure TScTask.RaiseException;
var ExceptObject : TObject;
begin
  if FatalException <> nil then
  begin
    ExceptObject := FFatalException;
    FFatalException := nil;
    raise ExceptObject;
  end;
end;

{---------------------------}
{ TScCustomStackQueue class }
{---------------------------}

{*
  Cr�e une liste de t�ches
  @param ADefaultFreeOnFinished   Valeur de la propri�t� DefaultFreeOnFinished
  @param ATerminateOnException    Valeur de la propri�t� TerminateOnException
*}
constructor TScCustomTaskQueue.Create(ADefaultFreeOnFinished : boolean = True;
  ATerminateOnException : boolean = True);
begin
  inherited Create(True);

  FWaitingQueue := TScWaitingObjectQueue.Create;
  FCriticalSection := TCriticalSection.Create;

  FDefaultFreeOnFinished := ADefaultFreeOnFinished;
  FTerminateOnException := ATerminateOnException;

  FReady := True;
end;

{*
  [@inheritDoc]
*}
destructor TScCustomTaskQueue.Destroy;
begin
  FCriticalSection.Free;
  FWaitingQueue.Free;
  inherited;
end;

{*
  Ajoute une t�che � la file
  @param Task   T�che � ajouter
*}
procedure TScCustomTaskQueue.Push(Task : TScTask);
begin
  CriticalSection.Acquire;
  try
    WaitingQueue.Push(Task);
  finally
    CriticalSection.Release;
  end;

  FReady := False;
  if Suspended then
    Resume;
end;

{*
  Retire une t�che de la file d'attente
  @param Task   T�che � retirer
*}
procedure TScCustomTaskQueue.Cancel(Task : TScTask);
begin
  CriticalSection.Acquire;
  try
    WaitingQueue.Cancel(Task);
    Task.Finish;
  finally
    CriticalSection.Release;
  end;
end;

{*
  R�cup�re la t�che suivante dans la file d'attente
  @return La prochaine t�che dans la file, ou nil si aucune t�che
*}
function TScCustomTaskQueue.Pop : TScTask;
begin
  CriticalSection.Acquire;
  try
    if WaitingQueue.Count = 0 then Result := nil else
    begin
      Result := WaitingQueue.Pop as TScTask;
      Result.FState := tsProcessing;
    end;
  finally
    CriticalSection.Release;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TScCustomTaskQueue.Execute;
var Task : TScTask;
    ExceptObject : TObject;
begin
  try
    while not Terminated do
    begin
      // Fetch the following task
      Task := Pop;
      if Task = nil then
      begin
        FReady := True;
        Suspend;
        Continue;
      end;

      // Execute the action
      try
        ExceptObject := nil;
        Task.Execute;
      except
        ExceptObject := AcquireExceptionObject;
      end;

      // Finish the action
      CriticalSection.Acquire;
      try
        Task.Finish(ExceptObject);
      finally
        CriticalSection.Release;
      end;

      // Terminate in case of fatal exception
      if TerminateOnException then
        Task.RaiseException;
    end;
  finally
    // Cancel all tasks which were not handled
    CriticalSection.Acquire;
    try
      while WaitingQueue.Count > 0 do
        (WaitingQueue.Pop as TScTask).Finish;
    finally
      CriticalSection.Release;
    end;

    // Mark as ready
    FReady := True;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TScCustomTaskQueue.BeforeDestruction;
begin
  inherited;

  Terminate;
  if Suspended then
    Resume;
  WaitFor;
end;

{----------------------------}
{ TScCustomMessageTask class }
{----------------------------}

{*
  Cr�e une nouvelle t�che de message
  @param AOwner            File de t�ches propri�taire
  @param AMsg              Message
  @param AMsgSize          Taille du message
  @param ADestObj          Objet auquel d�livrer le message
  @param AFreeOnFinished   Valeur de la propri�t� FreeOnFinished
*}
constructor TScCustomMessageTask.Create(AOwner : TScCustomTaskQueue;
  const AMsg; AMsgSize : integer; ADestObj : TObject;
  AFreeOnFinished : boolean);
begin
  inherited Create(AOwner, AFreeOnFinished);

  GetMem(FMsg, AMsgSize);
  Move(Msg, FMsg^, AMsgSize);
  FMsgSize := AMsgSize;
  FDestObj := ADestObj;
end;

{*
  Cr�e une nouvelle t�che de message
  @param AOwner     File de t�ches propri�taire
  @param AMsg       Message
  @param AMsgSize   Taille du message
  @param ADestObj   Objet auquel d�livrer le message
*}
constructor TScCustomMessageTask.Create(AOwner : TScCustomTaskQueue;
  const AMsg; AMsgSize : integer; ADestObj : TObject);
begin
  Create(AOwner, AMsg, AMsgSize, ADestObj, AOwner.DefaultFreeOnFinished);
end;

{*
  [@inheritDoc]
*}
destructor TScCustomMessageTask.Destroy;
begin
  FreeMem(FMsg);
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TScCustomMessageTask.Execute;
begin
  DestObj.Dispatch(FMsg^);
end;

{*
  R�cup�re une copie du message
  @param Msg   Emplacement o� copier le message
*}
procedure TScCustomMessageTask.GetMsg(out Msg);
begin
  Move(FMsg^, Msg, MsgSize);
end;

{----------------------}
{ TScMessageTask class }
{----------------------}

{*
  [@inheritDoc]
*}
function TScMessageTask.Cancel(ForceFree : boolean = False) : boolean;
begin
  Result := inherited Cancel(ForceFree);
end;

{*
  [@inheritDoc]
*}
procedure TScMessageTask.GetMsg(out Msg);
begin
  inherited;
end;

end.

