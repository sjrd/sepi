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
