Around line 1207 in Delphi 2009
----------
  TBalloonFlags = (bfNone = NIIF_NONE, bfInfo = NIIF_INFO,
    bfWarning = NIIF_WARNING, bfError = NIIF_ERROR);
----------
  TBalloonFlags = type Byte; const bfNone = TBalloonFlags(NIIF_NONE); bfInfo = TBalloonFlags(NIIF_INFO);
    bfWarning = TBalloonFlags(NIIF_WARNING); bfError = TBalloonFlags(NIIF_ERROR); type
----------

