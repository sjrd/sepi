Around line 296 in Delphi 2009
----------
  class var
    // Hook this function to return an opaque data structure that contains stack information
    // for the given exception information record. This function will be called when the
    // exception is about to be raised or if this is an external exception such as an
    // Access Violation, called soon after the object is created.
    GetExceptionStackInfoProc: function (P: PExceptionRecord): Pointer;
    // This function is called to return a string representation of the above opaque
    // data structure
    GetStackInfoStringProc: function (Info: Pointer): string;
    // This function is called when the destructor is called to clean up any data associated
    // with the given opaque data structure.
    CleanUpStackInfoProc: procedure (Info: Pointer);
    // Use this function to raise an exception instance from within an exception handler and
    // you want to "acquire" the active exception and chain it to the new exception and preserve
    // the context. This will cause the FInnerException field to get set with the exception
    // in currently in play.
    // You should only call this procedure from within an except block where the this new
    // exception is expected to be handled elsewhere.
    class procedure RaiseOuterException(E: Exception); static;
    // Provide another method that does the same thing as RaiseOuterException, but uses the
    // C++ vernacular of "throw"
    class procedure ThrowOuterException(E: Exception); static;
----------
----------

Around line 3375 in Delphi 2009
----------
    private const
      DefaultCapacity = $10;
----------
----------

Around line 3472 in Delphi 2009
----------
    class var
      FASCIIEncoding: TEncoding;
      FBigEndianUnicodeEncoding: TEncoding;
      FDefaultEncoding: TEncoding;
      FUnicodeEncoding: TEncoding;
      FUTF7Encoding: TEncoding;
      FUTF8Encoding: TEncoding;
----------
----------

Around line 3520 in Delphi 2009
----------
    class property ASCII: TEncoding read GetASCII;
    class property BigEndianUnicode: TEncoding read GetBigEndianUnicode;
    class property Default: TEncoding read GetDefault;
    property IsSingleByte: Boolean read FIsSingleByte;
    class property Unicode: TEncoding read GetUnicode;
    class property UTF7: TEncoding read GetUTF7;
    class property UTF8: TEncoding read GetUTF8;
----------
    property IsSingleByte: Boolean read FIsSingleByte;
----------

Around line 3601 in Delphi 2009
----------
// Generic Anonymous method declarations
type
  TProc = reference to procedure;
  TProc<T> = reference to procedure (Arg1: T);
  TProc<T1,T2> = reference to procedure (Arg1: T1; Arg2: T2);
  TProc<T1,T2,T3> = reference to procedure (Arg1: T1; Arg2: T2; Arg3: T3);
  TProc<T1,T2,T3,T4> = reference to procedure (Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4);

  TFunc<TResult> = reference to function: TResult;
  TFunc<T,TResult> = reference to function (Arg1: T): TResult;
  TFunc<T1,T2,TResult> = reference to function (Arg1: T1; Arg2: T2): TResult;
  TFunc<T1,T2,T3,TResult> = reference to function (Arg1: T1; Arg2: T2; Arg3: T3): TResult;
  TFunc<T1,T2,T3,T4,TResult> = reference to function (Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4): TResult;

  TPredicate<T> = reference to function (Arg1: T): Boolean;
----------
----------

