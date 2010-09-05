Around line 267 in Delphi 2010
----------
    class constructor Create;
    class destructor Destroy;
----------
----------

Around line 521 in Delphi 2009
----------
  EmptyStr: string = '';
  NullStr: PString = @EmptyStr;

  EmptyWideStr: WideString = '';
  NullWideStr: PWideString = @EmptyWideStr;
----------
  EmptyStr: string = '';
  NullStr: PString;

  EmptyWideStr: WideString = '';
  NullWideStr: PWideString;
----------

Around line 527 in Delphi 2009
----------
  EmptyAnsiStr: AnsiString = '';
  NullAnsiStr: PAnsiString = @EmptyAnsiStr;
----------
  EmptyAnsiStr: AnsiString = '';
  NullAnsiStr: PAnsiString;
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

