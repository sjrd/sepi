@SET RESDIR=$(SEPISDK)\resources\
@SET OUTDIR=$(SEPISDK)\source\run

SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiRTLSysImport\ SepiRTLSysImport.csv --skip-if-not-exists %1 %2 %3 %4 %5 %6 %7 %8 %9
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiWinTypesImport\ SepiWinTypesImport.csv --skip-if-not-exists --lazy-load=true --exclude-routines %1 %2 %3 %4 %5 %6 %7 %8 %9
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiRTLCommonImport\ SepiRTLCommonImport.csv --skip-if-not-exists %1 %2 %3 %4 %5 %6 %7 %8 %9
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiSCLImport\ SepiSCLImport.csv %1 %2 %3 %4 %5 %6 %7 %8 %9
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiVCLStdImport\ SepiVCLStdImport.csv --skip-if-not-exists %1 %2 %3 %4 %5 %6 %7 %8 %9
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiSVCLImport\ SepiSVCLImport.csv %1 %2 %3 %4 %5 %6 %7 %8 %9
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiSDLImport\ SepiSDLImport.csv %1 %2 %3 %4 %5 %6 %7 %8 %9
