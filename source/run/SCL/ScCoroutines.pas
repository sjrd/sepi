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
  Classes de gestion de coroutines
  ScCoroutines propose deux classes principales. TCustomCoroutine est une
  classe abstraite gérant des coroutines. Il faut la sucharger à la manière
  dont on surcharge TThread pour avoir une coroutine concrète. TCoroutine est
  une implémentation de celle-ci qui prend une méthode en paramètre, et exécute
  celle-ci comme coroutine.
  Cette unité a besoin d'un test sous Windows 95/98/Me pour l'agrandissement de
  la pile, car PAGE_GUARD n'est pas supporté dans ces versions.
  @author sjrd, sur une idée de Bart van der Werf
  @version 1.0
*}
unit ScCoroutines;

interface

uses
  Windows, SysUtils, Classes;

const
  /// Taille minimale d'allocation en une fois de la pile
  MinAllocStackBy = $2000; // > 4096, current buffer size in RTL

  /// Taille de pile par défaut
  DefaultStackSize = $10000;

resourcestring
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

  PTIB = ^TTIB;
  TTIB = packed record
    SEH: Pointer;
    StackTop: Pointer;
    StackBottom: Pointer;
  end;

  TRunningFrame = record
    SEH: Pointer;
    StackTop: Pointer;
    StackBottom: Pointer;
    StackPtr: Pointer;
    InstructionPtr: Pointer;
  end;

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

    La taille de pile doit être un multiple strict (2 fois ou plus) de la plus
    grande valeur entre a) la taille de page du système (en général 4096) et
    b) MinAllocStackBy.
    Toutefois, cela reste peu, et une taille recommandée est donnée par
    DefaultStackSize. Vous ne devriez en changer que si celle par défaut ne
    vous convient pas.

    @author sjrd, sur une idée de Bart van der Werf
    @version 1.0
  *}
  TCustomCoroutine = class(TObject)
  private
    FStackSize: Cardinal;  /// Taille maximale de la pile
    FStackBuffer: Pointer; /// Pile virtuelle totale
    FStack: Pointer;       /// Début de la pile de la coroutine

    FLoop: TCoroutineLoop; /// Type de boucle de la coroutine

    FCoroutineRunning: Boolean; /// True si la coroutine est cours d'exécution
    FTerminating: Boolean;      /// True si la coroutine doit se terminer
    FTerminated: Boolean;       /// True si la coroutine est terminée

    FCoroutineFrame: TRunningFrame; /// Cadre d'exécution de la coroutine
    FCallerFrame: TRunningFrame;    /// Cadre d'exécution de l'appelant

    FExceptObject: TObject;  /// Objet exception déclenchée par la coroutine
    FExceptAddress: Pointer; /// Adresse de déclenchement de l'exception

    procedure InitCoroutine;
    procedure Main;
    procedure SwitchRunningFrame;
    procedure Terminate;
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
    constructor Create(ALoop: TCoroutineLoop = clNoLoop;
      StackSize: Cardinal = DefaultStackSize);
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
      ALoop: TCoroutineLoop = clNoLoop;
      StackSize: Cardinal = DefaultStackSize);

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

var
  PageSize: Cardinal = 4096;
  Default8087CWAddress: PWord;

{-----------------}
{ Global routines }
{-----------------}

{*
  Initialise les variables globales
*}
procedure InitGlobalVars;
var
  SystemInfo: TSystemInfo;
begin
  GetSystemInfo(SystemInfo);
  PageSize := SystemInfo.dwPageSize;
  Default8087CWAddress := @Default8087CWAddress;
end;

{------------------------------------}
{ Global routines used by TCoroutine }
{------------------------------------}

{*
  Restaure un état serein d'exécution de code Delphi et trouve le TIB
  @return Adresse linéaire du TIB
*}
function CleanUpAndGetTIB: PTIB;
const
  TIBSelfPointer = $18;
asm
        // Clear Direction flag
        CLD

        // Reinitialize the FPU - see System._FpuInit
        FNINIT
        FWAIT
        MOV     EAX,Default8087CWAddress
        FLDCW   [EAX]

        // Get TIB
        MOV     EAX,TIBSelfPointer
        MOV     EAX,FS:[EAX]
end;

{*
  Pop tous les registres de la pile
  PopRegisters est utilisée comme point de retour dans SaveRunningFrame.
*}
procedure PopRegisters;
asm
        POPAD
end;

{*
  Sauvegarde le cadre d'exécution courant
  @param TIB     Pointeur sur le TIB
  @param Frame   Où stocker le cadre d'exécution
  @return Pointeur sur le TIB
*}
function SaveRunningFrame(TIB: PTIB; var Frame: TRunningFrame): PTIB;
asm
        { ->    EAX     Pointer to TIB
                EDX     Pointer to frame
          <-    EAX     Pointer to TIB   }

        // TIB
        MOV     ECX,[EAX].TTIB.SEH
        MOV     [EDX].TRunningFrame.SEH,ECX
        MOV     ECX,[EAX].TTIB.StackTop
        MOV     [EDX].TRunningFrame.StackTop,ECX
        MOV     ECX,[EAX].TTIB.StackBottom
        MOV     [EDX].TRunningFrame.StackBottom,ECX

        // ESP
        LEA     ECX,[ESP+4] // +4 because of return address
        MOV     [EDX].TRunningFrame.StackPtr,ECX

        // Return address
        MOV     [EDX].TRunningFrame.InstructionPtr,OFFSET PopRegisters
end;

{*
  Met en place un cadre d'exécution
  Cette procédure ne retourne jamais : elle continue l'exécution à
  l'instruction pointée par Frame.InstructionPtr.
  @param TIB     Pointeur sur le TIB
  @param Frame   Informations sur le cadre à mettre en place
*}
procedure SetupRunningFrame(TIB: PTIB; const Frame: TRunningFrame);
asm
        { Make sure you do a *JMP* to this procedure, not a *CALL*, because it
          won't get back and musn't get the return address in the stack. }

        { ->    EAX     Pointer to TIB
                EDX     Pointer to frame
                EBX     Value for EAX just before the jump }

        // TIB
        MOV     ECX,[EDX].TRunningFrame.SEH
        MOV     [EAX].TTIB.SEH,ECX
        MOV     ECX,[EDX].TRunningFrame.StackBottom
        MOV     [EAX].TTIB.StackBottom,ECX
        MOV     ECX,[EDX].TRunningFrame.StackTop
        MOV     [EAX].TTIB.StackTop,ECX

        // ESP
        MOV     ESP,[EDX].TRunningFrame.StackPtr

        // Jump to the instruction
        MOV     EAX,EBX
        MOV     ECX,[EDX].TRunningFrame.InstructionPtr
        JMP     ECX
end;

{------------------------}
{ TCustomCoroutine class }
{------------------------}

{*
  Crée une coroutine avec une taille de pile donnée
  @param ALoop       Type de boucle de la coroutine (défaut : clNoLoop)
  @param StackSize   Taille de la pile (défaut : DefaultStackSize)
*}
constructor TCustomCoroutine.Create(ALoop: TCoroutineLoop = clNoLoop;
  StackSize: Cardinal = DefaultStackSize);
var
  AllocStackBy: Cardinal;
begin
  inherited Create;

  // Compute AllocStackBy
  AllocStackBy := PageSize;
  if AllocStackBy < MinAllocStackBy then
    AllocStackBy := MinAllocStackBy;

  // Adapt stack size
  if StackSize < 2*AllocStackBy then
    StackSize := 2*AllocStackBy
  else if StackSize mod AllocStackBy <> 0 then
    StackSize := StackSize - (StackSize mod AllocStackBy) + AllocStackBy;

  // Reserve stack address space
  FStackSize := StackSize;
  FStackBuffer := VirtualAlloc(nil, FStackSize, MEM_RESERVE, PAGE_READWRITE);
  if not Assigned(FStackBuffer) then
    RaiseLastOSError;
  FStack := Pointer(Cardinal(FStackBuffer) + FStackSize);

  // Allocate base stack
  if not Assigned(VirtualAlloc(Pointer(Cardinal(FStack) - AllocStackBy),
    AllocStackBy, MEM_COMMIT, PAGE_READWRITE)) then
    RaiseLastOSError;
  if not Assigned(VirtualAlloc(Pointer(Cardinal(FStack) - 2*AllocStackBy),
    AllocStackBy, MEM_COMMIT, PAGE_READWRITE or PAGE_GUARD)) then
    RaiseLastOSError;

  // Set up configuration
  FLoop := ALoop;

  // Set up original state
  FCoroutineRunning := False;
  FTerminating := False;
  FTerminated := False;

  // Initialize coroutine
  InitCoroutine;
end;

{*
  Détruit l'instance
*}
destructor TCustomCoroutine.Destroy;
begin
  // Release stack address space
  if Assigned(FStackBuffer) then
    if not VirtualFree(FStackBuffer, 0, MEM_RELEASE) then
      RaiseLastOSError;

  inherited;
end;

{*
  Initialise la coroutine avant sa première exécution
*}
procedure TCustomCoroutine.InitCoroutine;
begin
  with FCoroutineFrame do
  begin
    SEH := nil;
    StackTop := FStack;
    StackBottom := FStackBuffer;
    StackPtr := FStack;
    InstructionPtr := @TCustomCoroutine.Main;
  end;

  FExceptObject := nil;
end;

{*
  Méthode principale de la coroutine
*}
procedure TCustomCoroutine.Main;
begin
  if not Terminating then
  try
    repeat
      Execute;
      if (Loop = clNextInvoke) and (not Terminating) then
        Yield;
    until (Loop = clNoLoop) or Terminating;
  except
    FExceptObject := AcquireExceptionObject;
    FExceptAddress := ExceptAddr;
  end;

  Terminate;
end;

{*
  Switche entre les deux cadres d'exécution (appelant-coroutine et vice versa)
*}
procedure TCustomCoroutine.SwitchRunningFrame;
asm
        { ->    EAX     Self }

        // Save all registers
        PUSHAD
        MOV     EBX,EAX

        // Get CoroutineRunning value into CF then switch it
        BTC     WORD PTR [EBX].TCoroutine.FCoroutineRunning,0

        // Get frame addresses
        LEA     ESI,[EBX].TCoroutine.FCoroutineFrame
        LEA     EDI,[EBX].TCoroutine.FCallerFrame
        JC      @@running // from BTC
        XCHG    ESI,EDI
@@running:

        // Clean up and get TIB
        CALL    CleanUpAndGetTIB

        // Save current running frame
        MOV     EDX,ESI
        CALL    SaveRunningFrame

        // Set up new running frame
        MOV     EDX,EDI
        JMP     SetupRunningFrame
end;

{*
  Termine la coroutine
*}
procedure TCustomCoroutine.Terminate;
asm
        { ->    EAX     Self }

        // Update state
        MOV     [EAX].TCoroutine.FTerminated,1
        MOV     [EAX].TCoroutine.FCoroutineRunning,0

        // Go back to caller running frame
        LEA     EDX,[EAX].TCoroutine.FCallerFrame
        CALL    CleanUpAndGetTIB
        JMP     SetupRunningFrame
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

  // Enter the coroutine
  SwitchRunningFrame;

  if Assigned(FExceptObject) then
  begin
    {$WARN SYMBOL_DEPRECATED OFF} // EStackOverflow is deprecated
    if FExceptObject is EStackOverflow then
    try
      // Reset guard in our stack - in case of upcoming call to Reset
      if not Assigned(VirtualAlloc(FStackBuffer, PageSize, MEM_COMMIT,
        PAGE_READWRITE or PAGE_GUARD)) then
        RaiseLastOSError;
    except
      FExceptObject.Free;
      raise;
    end;
    {$WARN SYMBOL_DEPRECATED ON}

    // Re-raise exception
    TempError := FExceptObject;
    FExceptObject := nil;
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

  SwitchRunningFrame;
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
    SwitchRunningFrame;
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
  @param StackSize        Taille de la pile (défaut : DefaultStackSize)
*}
constructor TCoroutine.Create(AExecuteMethod: TCoroutineMethod;
  ALoop: TCoroutineLoop = clNoLoop; StackSize: Cardinal = DefaultStackSize);
begin
  inherited Create(ALoop, StackSize);
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

initialization
  InitGlobalVars;
end.

