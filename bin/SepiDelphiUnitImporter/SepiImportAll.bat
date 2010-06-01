@SET RESDIR=$(SEPISDK)\resources\
@SET OUTDIR=$(SEPISDK)\source\run

SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiRTLSysImport\ Types.pas %1 %2 %3 %4 %5 %6 %7 %8 %9
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiRTLSysImport\ Windows.pas --lazy-load=true --exclude-routines %1 %2 %3 %4 %5 %6 %7 %8 %9

SepiImportAlmostAll %1 %2 %3 %4 %5 %6 %7 %8 %9
