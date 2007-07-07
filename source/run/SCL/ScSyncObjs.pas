unit ScSyncObjs;

interface

uses
  Classes, Contnrs, SyncObjs, ScLists;

type
  TScCustomTaskQueue = class;

  {*
    État d'une tâche
    - tsWaiting : en attente ;
    - tsProcessing : en cours ;
    - tsFinished : terminée ;
    - tsCanceled : annulée.
  *}
  TScTaskState = (tsWaiting, tsProcessing, tsFinished, tsCancelled);

  {*
    Tâche gérée par une instance de TScTaskQueue
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TScTask = class
  private
    FOwner : TScCustomTaskQueue; /// File propriétaire de la tâche
    FState : TScTaskState;       /// État de la tâche
    FFreeOnFinished : boolean;   /// Se libère automatiquement

    FWaitForCount : integer; /// Nombre d'exécutions de WaitFor en cours

    FFatalException : TObject; /// Exception non interceptée

    procedure Finish(AFatalException : TObject = nil);
  protected
    {*
      Exécute l'action
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
    Gestionnaire de file (FIFO) de tâches
    TScCustomTaskQueue est une classe de base pour l'implémentation de files de
    tâches, qui s'exécutent de façon synchrone entre elles, mais asynchrone par
    rapport au contrôleur.
    Puisque TScCustomTaskQueue hérite de TThread au lieu de l'encapsuler, le
    contrôleur a plein accès sur le déroulement du thread de répartition.
    Un appel à la méthode Terminate mettra fin à la répartition des tâches,
    même si la file n'est pas vide.
    Pour attendre que la liste des messages en attente soit vide, utilisez la
    propriété Ready, et non la méthode WaitFor. Pour attendre qu'une tâche en
    particulier soit terminée, les descendants de TScCustomTaskQueue peuvent
    offrir l'accès aux objets TScTask, qui proposent une méthode WaitFor.
    Vous pouvez positionner la propriété TerminateOnException à False si vous
    ne voulez pas que le traitement des messages s'arrête en cas d'exception
    lors de l'un d'eux.
    Les descendants de TScTaskQueue ne devraient normalement pas réimplémenter
    la méthode Execute.
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TScCustomTaskQueue = class(TThread)
  private
    FWaitingQueue : TScWaitingObjectQueue; /// File des messages en attente
    FCriticalSection : TCriticalSection;   /// Section critique sur la liste

    /// Valeur par défaut de la propriété FreeOnFinished des tâches de la file
    FDefaultFreeOnFinished : boolean;

    /// Indique si le thread d'exécution doit se terminer en cas d'exception
    FTerminateOnException : boolean;

    FReady : boolean; /// Indique si la file a terminé son travail

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
    Gestionnaire de file (FIFO) de tâches
    TScTaskQueue propose une implémentation entièrement publique de
    TScCustomTaskQueue.
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TScTaskQueue = class(TScCustomTaskQueue)
  public
    property DefaultFreeOnFinished;
    property TerminateOnException;
    property Ready;
  end;

  {*
    Tâche de répartition de message
    TScCustomMessageTask fournit une implémentation protégée d'une tâche de
    dispatching de message.
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TScCustomMessageTask = class(TScTask)
  private
    FMsg : Pointer;     /// Pointeur sur le message
    FMsgSize : integer; /// Taille du message
    FDestObj : TObject; /// Objet auquel délivrer le message
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
    Tâche de répartition de message
    TScMessageTask est une version publique de TScCustomMessageTask.
    @author Sébastien Jean Robert Doeraene
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
  Incrémente un compteur de façon thread-safe
  @param Compteur à incrémenter
  @return Nouvelle valeur du compteur
*}
function InterlockedIncrement(var I : integer) : integer;
asm
        MOV     EDX,1
   LOCK XADD    [EAX],EDX
        LEA     EAX,[EDX+1]
end;

{*
  Décrémente un compteur de façon thread-safe
  @param Compteur à décrémenter
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
  Crée une tâche en spécifiant FreeOnFinished
  @param AOwner            File de tâches propriétaire
  @param AFreeOnFinished   Indique si doit se détruire automatiquement
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
  Crée une tâche
  @param AOwner   File de tâches propriétaire
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
  Termine la tâche
  @param AFatalException   Exception non interceptée
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
  Annule la tâche
  Cancel ne fait rien si la tâche est déjà en cours ou terminée
  @param ForceFree   À True, force la libération, même si FreeOnFinished = False
  @return True si réussi, False si la tâche était déjà en cours ou terminée
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
  Attend que la tâche soit terminée
  WaitFor peut être appelée sans risque sur une tâche dont la propriété
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
  Déclenche l'exception FatalException, si celle-ci est non nulle
  Utilisez RaiseException plutôt que "raise FatalException", ou vous courrez à
  la violation d'accès lors de la libération de l'objet TScTask.
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
  Crée une liste de tâches
  @param ADefaultFreeOnFinished   Valeur de la propriété DefaultFreeOnFinished
  @param ATerminateOnException    Valeur de la propriété TerminateOnException
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
  Ajoute une tâche à la file
  @param Task   Tâche à ajouter
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
  Retire une tâche de la file d'attente
  @param Task   Tâche à retirer
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
  Récupère la tâche suivante dans la file d'attente
  @return La prochaine tâche dans la file, ou nil si aucune tâche
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
  Crée une nouvelle tâche de message
  @param AOwner            File de tâches propriétaire
  @param AMsg              Message
  @param AMsgSize          Taille du message
  @param ADestObj          Objet auquel délivrer le message
  @param AFreeOnFinished   Valeur de la propriété FreeOnFinished
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
  Crée une nouvelle tâche de message
  @param AOwner     File de tâches propriétaire
  @param AMsg       Message
  @param AMsgSize   Taille du message
  @param ADestObj   Objet auquel délivrer le message
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
  Récupère une copie du message
  @param Msg   Emplacement où copier le message
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

