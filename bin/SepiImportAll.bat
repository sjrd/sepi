@SET RESDIR=$(SEPISDK)\resources\
@SET OUTDIR=$(SEPISDK)\source\run

SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiRTLSysImport\ Types.pas
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiRTLSysImport\ Windows.pas --lazy-load=true --exclude-routines
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiRTLSysImport\ SepiRTLSysImport.csv
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiWinTypesImport\ SepiWinTypesImport.csv --lazy-load=true --exclude-routines
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiRTLCommonImport\ SepiRTLCommonImport.csv
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiSCLImport\ SepiSCLImport.csv
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiVCLStdImport\ SepiVCLStdImport.csv
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiSVCLImport\ SepiSVCLImport.csv
SepiDelphiUnitImporter -r %RESDIR% -o %OUTDIR%\SepiSDLImport\ SepiSDLImport.csv
