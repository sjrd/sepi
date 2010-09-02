Around line 6 in Delphi 2010
----------
  TTimeSpan = record
  private
    FTicks: Int64;
----------
  TTimeSpan = record

  FTicks: Int64; {$IFNDEF SEPIPARSER}
----------

Around line 11258 in Delphi 2010
----------
    class property Zero: TTimeSpan read FZero;
  end;
----------
    class property Zero: TTimeSpan read FZero; {$ENDIF}
  end;
----------
