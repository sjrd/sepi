Around line 140 in Delphi 2010
----------
  TListSortCompareFunc = reference to function (Item1, Item2: Pointer): Integer;
----------
  TListSortCompareFunc = interface(IInterface) end; // keep init/finit the same
----------

Around line 1207 in Delphi 2009
----------
  TThreadProcedure = reference to procedure;
----------
  TThreadProcedure = interface(IInterface) end; // keep init/finit the same
----------

Around line 1256 in Delphi 2010
----------
    class constructor Create;
    class destructor Destroy;
----------
----------

Around line 1274 in Delphi 2010
----------
  private class threadvar
    FCurrentThread: TThread;
----------
----------
