Around line 31 in Delphi 2009
----------
  TCriticalSectionHelper = record helper for TRTLCriticalSection
    procedure Initialize; inline;
    procedure Destroy; inline;
    procedure Free; inline;
    procedure Enter; inline;
    procedure Leave; inline;
    function TryEnter: Boolean; inline;
  end;

  TConditionVariableHelper = record helper for TRTLConditionVariable
  public
    class function Create: TRTLConditionVariable; static;
    procedure Free; inline;
    function SleepCS(var CriticalSection: TRTLCriticalSection; dwMilliseconds: DWORD): Boolean;
    procedure Wake;
    procedure WakeAll;
  end;
----------
----------

