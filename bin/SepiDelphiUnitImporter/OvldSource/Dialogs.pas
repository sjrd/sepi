Around line 542 in Delphi 2009
----------
  TFileDialogOverwriteResponse = (forDefault = FDEOR_DEFAULT,
    forAccept = FDEOR_ACCEPT, forRefuse = FDEOR_REFUSE);
  TFileDialogShareViolationResponse = (fsrDefault = FDESVR_DEFAULT,
    fsrAccept = FDESVR_ACCEPT, fsrRefuse = FDESVR_REFUSE);
----------
  TFileDialogOverwriteResponse = type Byte; const forDefault = TFileDialogOverwriteResponse(FDEOR_DEFAULT);
    forAccept = TFileDialogOverwriteResponse(FDEOR_ACCEPT); forRefuse = TFileDialogOverwriteResponse(FDEOR_REFUSE); type
  TFileDialogShareViolationResponse = type Byte; const fsrDefault = TFileDialogShareViolationResponse(FDESVR_DEFAULT);
    fsrAccept = TFileDialogShareViolationResponse(FDESVR_ACCEPT); fsrRefuse = TFileDialogShareViolationResponse(FDESVR_REFUSE); type
----------

