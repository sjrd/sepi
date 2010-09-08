Around line 1917 in Delphi 2009
----------
procedure _FindDynaInst;
procedure _FindDynaClass;
----------
function _FindDynaInst(Obj: TObject; DMTIndex: Integer): Pointer;
function _FindDynaClass(Cls: TClass; DMTIndex: Integer): Pointer;
----------

Around line 293 in Delphi 2010
----------
  TArray<T> = array of T;
----------
----------

Around line 359 in Delphi 2009
----------
  TMonitor = record
  strict private
    type
      PWaitingThread = ^TWaitingThread;
      TWaitingThread = record
        Next: PWaitingThread;
        Thread: Cardinal;
        WaitEvent: Pointer;
      end;
    var
      FLockCount: Integer;
      FRecursionCount: Integer;
      FOwningThread: Cardinal;
      FLockEvent: Pointer;
      FSpinCount: Integer;
      FWaitQueue: PWaitingThread;
    procedure QueueWaiter(var WaitingThread: TWaitingThread);
    procedure RemoveWaiter(var WaitingThread: TWaitingThread);
    function DequeueWaiter: PWaitingThread;
    function GetEvent: Pointer;
    function CheckOwningThread: Cardinal;
    class procedure CheckMonitorSupport; static; inline;
    class function Create: PMonitor; static;
    // Make sure the following Destroy overload is always
    // listed first since it is called from an asm block
    // and there is no overload-resolution done from an
    // assembler symbol reference
    class procedure Destroy(AObject: TObject); overload; static;
    class function GetFieldAddress(AObject: TObject): PPMonitor; inline; static;
    class function GetMonitor(AObject: TObject): PMonitor; static;
    procedure Destroy; overload;
    function Enter(Timeout: Cardinal): Boolean; overload;
    procedure Exit; overload;
    function TryEnter: Boolean; overload;
    function Wait(Timeout: Cardinal): Boolean; overload;
    procedure Pulse; overload;
    procedure PulseAll; overload;
  public
    class procedure SetSpinCount(AObject: TObject; ASpinCount: Integer); static;
    class procedure Enter(AObject: TObject); overload; static; inline;
    class function Enter(AObject: TObject; Timeout: Cardinal): Boolean; overload; static;
    class procedure Exit(AObject: TObject); overload; static;
    class function TryEnter(AObject: TObject): Boolean; overload; static;
    class function Wait(AObject: TObject; Timeout: Cardinal): Boolean; overload; static;
    class procedure Pulse(AObject: TObject); overload; static;
    class procedure PulseAll(AObject: TObject); overload; static;
  end;
----------
  TMonitor = record
    FLockCount: Integer;
    FRecursionCount: Integer;
    FOwningThread: Cardinal;
    FLockEvent: Pointer;
    FSpinCount: Integer;
    FWaitQueue: Pointer;
  end;
----------

Around line 455 in Delphi 2009
----------
  IEnumerator<T> = interface(IEnumerator)
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface(IEnumerable)
    function GetEnumerator: IEnumerator<T>;
  end;
----------
----------

Around line 468 in Delphi 2009
----------
  IComparable<T> = interface
    function CompareTo(Value: T): Integer;
  end;

  IEquatable<T> = interface
    function Equals(Value: T): Boolean;
  end;
----------
----------

Around line 495 in Delphi 2010
----------
  IComparable<T> = interface(IComparable)
    function CompareTo(Value: T): Integer;
  end;

  IEquatable<T> = interface(IInterface)
    function Equals(Value: T): Boolean;
  end;
----------
----------

Around line 1236 in Delphi 2009
----------
  HeapAllocFlags: Word platform = 2;   { Heap allocation flags, gmem_Moveable }
  DebugHook: Byte platform = 0;        { 1 to notify debugger of non-Delphi exceptions
                                >1 to notify debugger of exception unwinding }
  JITEnable: Byte platform = 0;        { 1 to call UnhandledExceptionFilter if the exception
                                is not a Pascal exception.
                                >1 to call UnhandledExceptionFilter for all exceptions }
  NoErrMsg: Boolean platform = False;  { True causes the base RTL to not display the message box
                                when a run-time error occurs }
----------
  HeapAllocFlags: Word = 2;   { Heap allocation flags, gmem_Moveable }
  DebugHook: Byte = 0;        { 1 to notify debugger of non-Delphi exceptions
                                >1 to notify debugger of exception unwinding }
  JITEnable: Byte = 0;        { 1 to call UnhandledExceptionFilter if the exception
                                is not a Pascal exception.
                                >1 to call UnhandledExceptionFilter for all exceptions }
  NoErrMsg: Boolean = False;  { True causes the base RTL to not display the message box
                                when a run-time error occurs }
----------

Around line 1250 in Delphi 2009
----------
  CoreDumpEnabled: Boolean platform = False;
----------
  CoreDumpEnabled: Boolean = False;
----------

Around line 2404 in Delphi 2010
----------
  UnloadDelayLoadedDLLPtr: Pointer = @__FUnloadDelayLoadedDLL;
  DelayLoadHelper: Pointer = @__delayLoadHelper;
  pfnDliNotifyHook: Pointer = @___pfnDliNotifyHook;
  pfnDliFailureHook: Pointer = @___pfnDliFailureHook;
----------
  UnloadDelayLoadedDLLPtr: Pointer;
  DelayLoadHelper: Pointer;
  pfnDliNotifyHook: Pointer;
  pfnDliFailureHook: Pointer;
----------
