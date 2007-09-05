{*
  Importe l'unité WinSpool dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsWinSpoolTypes;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, WinSpool;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------------------}
{ _PRINTER_INFO_1A import }
{-------------------------}

function SepiImport_PRINTER_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_1A', False, True);

  with Result do
  begin
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('pDescription', 'PAnsiChar');
    AddField('pName', 'PAnsiChar');
    AddField('pComment', 'PAnsiChar');

    Complete;
  end;
end;

{-------------------------}
{ _PRINTER_INFO_1W import }
{-------------------------}

function SepiImport_PRINTER_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_1W', False, True);

  with Result do
  begin
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('pDescription', 'PWideChar');
    AddField('pName', 'PWideChar');
    AddField('pComment', 'PWideChar');

    Complete;
  end;
end;

{-------------------------}
{ _PRINTER_INFO_2A import }
{-------------------------}

function SepiImport_PRINTER_INFO_2A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_2A', False, True);

  with Result do
  begin
    AddField('pServerName', 'PAnsiChar');
    AddField('pPrinterName', 'PAnsiChar');
    AddField('pShareName', 'PAnsiChar');
    AddField('pPortName', 'PAnsiChar');
    AddField('pDriverName', 'PAnsiChar');
    AddField('pComment', 'PAnsiChar');
    AddField('pLocation', 'PAnsiChar');
    AddField('pDevMode', 'PDeviceModeA');
    AddField('pSepFile', 'PAnsiChar');
    AddField('pPrintProcessor', 'PAnsiChar');
    AddField('pDatatype', 'PAnsiChar');
    AddField('pParameters', 'PAnsiChar');
    AddField('pSecurityDescriptor', 'PSecurityDescriptor');
    AddField('Attributes', System.TypeInfo(DWORD));
    AddField('Priority', System.TypeInfo(DWORD));
    AddField('DefaultPriority', System.TypeInfo(DWORD));
    AddField('StartTime', System.TypeInfo(DWORD));
    AddField('UntilTime', System.TypeInfo(DWORD));
    AddField('Status', System.TypeInfo(DWORD));
    AddField('cJobs', System.TypeInfo(DWORD));
    AddField('AveragePPM', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------------}
{ _PRINTER_INFO_2W import }
{-------------------------}

function SepiImport_PRINTER_INFO_2W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_2W', False, True);

  with Result do
  begin
    AddField('pServerName', 'PWideChar');
    AddField('pPrinterName', 'PWideChar');
    AddField('pShareName', 'PWideChar');
    AddField('pPortName', 'PWideChar');
    AddField('pDriverName', 'PWideChar');
    AddField('pComment', 'PWideChar');
    AddField('pLocation', 'PWideChar');
    AddField('pDevMode', 'PDeviceModeW');
    AddField('pSepFile', 'PWideChar');
    AddField('pPrintProcessor', 'PWideChar');
    AddField('pDatatype', 'PWideChar');
    AddField('pParameters', 'PWideChar');
    AddField('pSecurityDescriptor', 'PSecurityDescriptor');
    AddField('Attributes', System.TypeInfo(DWORD));
    AddField('Priority', System.TypeInfo(DWORD));
    AddField('DefaultPriority', System.TypeInfo(DWORD));
    AddField('StartTime', System.TypeInfo(DWORD));
    AddField('UntilTime', System.TypeInfo(DWORD));
    AddField('Status', System.TypeInfo(DWORD));
    AddField('cJobs', System.TypeInfo(DWORD));
    AddField('AveragePPM', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{------------------------}
{ _PRINTER_INFO_3 import }
{------------------------}

function SepiImport_PRINTER_INFO_3(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_3', False, True);

  with Result do
  begin
    AddField('pSecurityDescriptor', 'PSecurityDescriptor');

    Complete;
  end;
end;

{-------------------------}
{ _PRINTER_INFO_4A import }
{-------------------------}

function SepiImport_PRINTER_INFO_4A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_4A', False, True);

  with Result do
  begin
    AddField('pPrinterName', 'PAnsiChar');
    AddField('pServerName', 'PAnsiChar');
    AddField('Attributes', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------------}
{ _PRINTER_INFO_4W import }
{-------------------------}

function SepiImport_PRINTER_INFO_4W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_4W', False, True);

  with Result do
  begin
    AddField('pPrinterName', 'PWideChar');
    AddField('pServerName', 'PWideChar');
    AddField('Attributes', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------------}
{ _PRINTER_INFO_5A import }
{-------------------------}

function SepiImport_PRINTER_INFO_5A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_5A', False, True);

  with Result do
  begin
    AddField('pPrinterName', 'PAnsiChar');
    AddField('pPortName', 'PAnsiChar');
    AddField('Attributes', System.TypeInfo(DWORD));
    AddField('DeviceNotSelectedTimeout', System.TypeInfo(DWORD));
    AddField('TransmissionRetryTimeout', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------------}
{ _PRINTER_INFO_5W import }
{-------------------------}

function SepiImport_PRINTER_INFO_5W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_5W', False, True);

  with Result do
  begin
    AddField('pPrinterName', 'PWideChar');
    AddField('pPortName', 'PWideChar');
    AddField('Attributes', System.TypeInfo(DWORD));
    AddField('DeviceNotSelectedTimeout', System.TypeInfo(DWORD));
    AddField('TransmissionRetryTimeout', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{------------------------}
{ _PRINTER_INFO_6 import }
{------------------------}

function SepiImport_PRINTER_INFO_6(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_INFO_6', False, True);

  with Result do
  begin
    AddField('dwStatus', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{---------------------}
{ _JOB_INFO_1A import }
{---------------------}

function SepiImport_JOB_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_JOB_INFO_1A', False, True);

  with Result do
  begin
    AddField('JobId', System.TypeInfo(DWORD));
    AddField('pPrinterName', 'PAnsiChar');
    AddField('pMachineName', 'PAnsiChar');
    AddField('pUserName', 'PAnsiChar');
    AddField('pDocument', 'PAnsiChar');
    AddField('pDatatype', 'PAnsiChar');
    AddField('pStatus', 'PAnsiChar');
    AddField('Status', System.TypeInfo(DWORD));
    AddField('Priority', System.TypeInfo(DWORD));
    AddField('Position', System.TypeInfo(DWORD));
    AddField('TotalPages', System.TypeInfo(DWORD));
    AddField('PagesPrinted', System.TypeInfo(DWORD));
    AddField('Submitted', 'TSystemTime');

    Complete;
  end;
end;

{---------------------}
{ _JOB_INFO_1W import }
{---------------------}

function SepiImport_JOB_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_JOB_INFO_1W', False, True);

  with Result do
  begin
    AddField('JobId', System.TypeInfo(DWORD));
    AddField('pPrinterName', 'PWideChar');
    AddField('pMachineName', 'PWideChar');
    AddField('pUserName', 'PWideChar');
    AddField('pDocument', 'PWideChar');
    AddField('pDatatype', 'PWideChar');
    AddField('pStatus', 'PWideChar');
    AddField('Status', System.TypeInfo(DWORD));
    AddField('Priority', System.TypeInfo(DWORD));
    AddField('Position', System.TypeInfo(DWORD));
    AddField('TotalPages', System.TypeInfo(DWORD));
    AddField('PagesPrinted', System.TypeInfo(DWORD));
    AddField('Submitted', 'TSystemTime');

    Complete;
  end;
end;

{---------------------}
{ _JOB_INFO_2A import }
{---------------------}

function SepiImport_JOB_INFO_2A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_JOB_INFO_2A', False, True);

  with Result do
  begin
    AddField('JobId', System.TypeInfo(DWORD));
    AddField('pPrinterName', 'PAnsiChar');
    AddField('pMachineName', 'PAnsiChar');
    AddField('pUserName', 'PAnsiChar');
    AddField('pDocument', 'PAnsiChar');
    AddField('pNotifyName', 'PAnsiChar');
    AddField('pDatatype', 'PAnsiChar');
    AddField('pPrintProcessor', 'PAnsiChar');
    AddField('pParameters', 'PAnsiChar');
    AddField('pDriverName', 'PAnsiChar');
    AddField('pDevMode', 'PDeviceModeA');
    AddField('pStatus', 'PAnsiChar');
    AddField('pSecurityDescriptor', 'PSECURITY_DESCRIPTOR');
    AddField('Status', System.TypeInfo(DWORD));
    AddField('Priority', System.TypeInfo(DWORD));
    AddField('Position', System.TypeInfo(DWORD));
    AddField('StartTime', System.TypeInfo(DWORD));
    AddField('UntilTime', System.TypeInfo(DWORD));
    AddField('TotalPages', System.TypeInfo(DWORD));
    AddField('Size', System.TypeInfo(DWORD));
    AddField('Submitted', 'TSystemTime');
    AddField('Time', System.TypeInfo(DWORD));
    AddField('PagesPrinted', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{---------------------}
{ _JOB_INFO_2W import }
{---------------------}

function SepiImport_JOB_INFO_2W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_JOB_INFO_2W', False, True);

  with Result do
  begin
    AddField('JobId', System.TypeInfo(DWORD));
    AddField('pPrinterName', 'PWideChar');
    AddField('pMachineName', 'PWideChar');
    AddField('pUserName', 'PWideChar');
    AddField('pDocument', 'PWideChar');
    AddField('pNotifyName', 'PWideChar');
    AddField('pDatatype', 'PWideChar');
    AddField('pPrintProcessor', 'PWideChar');
    AddField('pParameters', 'PWideChar');
    AddField('pDriverName', 'PWideChar');
    AddField('pDevMode', 'PDeviceModeW');
    AddField('pStatus', 'PWideChar');
    AddField('pSecurityDescriptor', 'PSECURITY_DESCRIPTOR');
    AddField('Status', System.TypeInfo(DWORD));
    AddField('Priority', System.TypeInfo(DWORD));
    AddField('Position', System.TypeInfo(DWORD));
    AddField('StartTime', System.TypeInfo(DWORD));
    AddField('UntilTime', System.TypeInfo(DWORD));
    AddField('TotalPages', System.TypeInfo(DWORD));
    AddField('Size', System.TypeInfo(DWORD));
    AddField('Submitted', 'TSystemTime');
    AddField('Time', System.TypeInfo(DWORD));
    AddField('PagesPrinted', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{--------------------}
{ _JOB_INFO_3 import }
{--------------------}

function SepiImport_JOB_INFO_3(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_JOB_INFO_3', False, True);

  with Result do
  begin
    AddField('JobId', System.TypeInfo(DWORD));
    AddField('NextJobId', System.TypeInfo(DWORD));
    AddField('Reserved', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{------------------------}
{ _ADDJOB_INFO_1A import }
{------------------------}

function SepiImport_ADDJOB_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_ADDJOB_INFO_1A', False, True);

  with Result do
  begin
    AddField('Path', 'PAnsiChar');
    AddField('JobId', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{------------------------}
{ _ADDJOB_INFO_1W import }
{------------------------}

function SepiImport_ADDJOB_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_ADDJOB_INFO_1W', False, True);

  with Result do
  begin
    AddField('Path', 'PWideChar');
    AddField('JobId', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{------------------------}
{ _DRIVER_INFO_1A import }
{------------------------}

function SepiImport_DRIVER_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DRIVER_INFO_1A', False, True);

  with Result do
  begin
    AddField('pName', 'PAnsiChar');

    Complete;
  end;
end;

{------------------------}
{ _DRIVER_INFO_1W import }
{------------------------}

function SepiImport_DRIVER_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DRIVER_INFO_1W', False, True);

  with Result do
  begin
    AddField('pName', 'PWideChar');

    Complete;
  end;
end;

{------------------------}
{ _DRIVER_INFO_2A import }
{------------------------}

function SepiImport_DRIVER_INFO_2A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DRIVER_INFO_2A', False, True);

  with Result do
  begin
    AddField('cVersion', System.TypeInfo(DWORD));
    AddField('pName', 'PAnsiChar');
    AddField('pEnvironment', 'PAnsiChar');
    AddField('pDriverPath', 'PAnsiChar');
    AddField('pDataFile', 'PAnsiChar');
    AddField('pConfigFile', 'PAnsiChar');

    Complete;
  end;
end;

{------------------------}
{ _DRIVER_INFO_2W import }
{------------------------}

function SepiImport_DRIVER_INFO_2W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DRIVER_INFO_2W', False, True);

  with Result do
  begin
    AddField('cVersion', System.TypeInfo(DWORD));
    AddField('pName', 'PWideChar');
    AddField('pEnvironment', 'PWideChar');
    AddField('pDriverPath', 'PWideChar');
    AddField('pDataFile', 'PWideChar');
    AddField('pConfigFile', 'PWideChar');

    Complete;
  end;
end;

{------------------------}
{ _DRIVER_INFO_3A import }
{------------------------}

function SepiImport_DRIVER_INFO_3A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DRIVER_INFO_3A', False, True);

  with Result do
  begin
    AddField('cVersion', System.TypeInfo(DWORD));
    AddField('pName', 'PAnsiChar');
    AddField('pEnvironment', 'PAnsiChar');
    AddField('pDriverPath', 'PAnsiChar');
    AddField('pDataFile', 'PAnsiChar');
    AddField('pConfigFile', 'PAnsiChar');
    AddField('pHelpFile', 'PAnsiChar');
    AddField('pDependentFiles', 'PAnsiChar');
    AddField('pMonitorName', 'PAnsiChar');
    AddField('pDefaultDataType', 'PAnsiChar');

    Complete;
  end;
end;

{------------------------}
{ _DRIVER_INFO_3W import }
{------------------------}

function SepiImport_DRIVER_INFO_3W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DRIVER_INFO_3W', False, True);

  with Result do
  begin
    AddField('cVersion', System.TypeInfo(DWORD));
    AddField('pName', 'PWideChar');
    AddField('pEnvironment', 'PWideChar');
    AddField('pDriverPath', 'PWideChar');
    AddField('pDataFile', 'PWideChar');
    AddField('pConfigFile', 'PWideChar');
    AddField('pHelpFile', 'PWideChar');
    AddField('pDependentFiles', 'PWideChar');
    AddField('pMonitorName', 'PWideChar');
    AddField('pDefaultDataType', 'PWideChar');

    Complete;
  end;
end;

{---------------------}
{ _DOC_INFO_1A import }
{---------------------}

function SepiImport_DOC_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DOC_INFO_1A', False, True);

  with Result do
  begin
    AddField('pDocName', 'PAnsiChar');
    AddField('pOutputFile', 'PAnsiChar');
    AddField('pDatatype', 'PAnsiChar');

    Complete;
  end;
end;

{---------------------}
{ _DOC_INFO_1W import }
{---------------------}

function SepiImport_DOC_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DOC_INFO_1W', False, True);

  with Result do
  begin
    AddField('pDocName', 'PWideChar');
    AddField('pOutputFile', 'PWideChar');
    AddField('pDatatype', 'PWideChar');

    Complete;
  end;
end;

{----------------------}
{ _FORM_INFO_1A import }
{----------------------}

function SepiImport_FORM_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_FORM_INFO_1A', False, True);

  with Result do
  begin
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('pName', 'PAnsiChar');
    AddField('Size', 'TSize');
    AddField('ImageableArea', 'TRect');

    Complete;
  end;
end;

{----------------------}
{ _FORM_INFO_1W import }
{----------------------}

function SepiImport_FORM_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_FORM_INFO_1W', False, True);

  with Result do
  begin
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('pName', 'PWideChar');
    AddField('Size', 'TSize');
    AddField('ImageableArea', 'TRect');

    Complete;
  end;
end;

{---------------------}
{ _DOC_INFO_2A import }
{---------------------}

function SepiImport_DOC_INFO_2A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DOC_INFO_2A', False, True);

  with Result do
  begin
    AddField('pDocName', 'PAnsiChar');
    AddField('pOutputFile', 'PAnsiChar');
    AddField('pDatatype', 'PAnsiChar');
    AddField('dwMode', System.TypeInfo(DWORD));
    AddField('JobId', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{---------------------}
{ _DOC_INFO_2W import }
{---------------------}

function SepiImport_DOC_INFO_2W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DOC_INFO_2W', False, True);

  with Result do
  begin
    AddField('pDocName', 'PWideChar');
    AddField('pOutputFile', 'PWideChar');
    AddField('pDatatype', 'PWideChar');
    AddField('dwMode', System.TypeInfo(DWORD));
    AddField('JobId', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{--------------------------------}
{ _PRINTPROCESSOR_INFO_1A import }
{--------------------------------}

function SepiImport_PRINTPROCESSOR_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTPROCESSOR_INFO_1A', False, True);

  with Result do
  begin
    AddField('pName', 'PAnsiChar');

    Complete;
  end;
end;

{--------------------------------}
{ _PRINTPROCESSOR_INFO_1W import }
{--------------------------------}

function SepiImport_PRINTPROCESSOR_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTPROCESSOR_INFO_1W', False, True);

  with Result do
  begin
    AddField('pName', 'PWideChar');

    Complete;
  end;
end;

{----------------------}
{ _PORT_INFO_1A import }
{----------------------}

function SepiImport_PORT_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PORT_INFO_1A', False, True);

  with Result do
  begin
    AddField('pName', 'PAnsiChar');

    Complete;
  end;
end;

{----------------------}
{ _PORT_INFO_1W import }
{----------------------}

function SepiImport_PORT_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PORT_INFO_1W', False, True);

  with Result do
  begin
    AddField('pName', 'PWideChar');

    Complete;
  end;
end;

{----------------------}
{ _PORT_INFO_2A import }
{----------------------}

function SepiImport_PORT_INFO_2A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PORT_INFO_2A', False, True);

  with Result do
  begin
    AddField('pPortName', 'PAnsiChar');
    AddField('pMonitorName', 'PAnsiChar');
    AddField('pDescription', 'PAnsiChar');
    AddField('fPortType', System.TypeInfo(DWORD));
    AddField('Reserved', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{----------------------}
{ _PORT_INFO_2W import }
{----------------------}

function SepiImport_PORT_INFO_2W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PORT_INFO_2W', False, True);

  with Result do
  begin
    AddField('pPortName', 'PWideChar');
    AddField('pMonitorName', 'PWideChar');
    AddField('pDescription', 'PWideChar');
    AddField('fPortType', System.TypeInfo(DWORD));
    AddField('Reserved', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{----------------------}
{ _PORT_INFO_3A import }
{----------------------}

function SepiImport_PORT_INFO_3A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PORT_INFO_3A', False, True);

  with Result do
  begin
    AddField('dwStatus', System.TypeInfo(DWORD));
    AddField('pszStatus', 'PAnsiChar');
    AddField('dwSeverity', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{----------------------}
{ _PORT_INFO_3W import }
{----------------------}

function SepiImport_PORT_INFO_3W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PORT_INFO_3W', False, True);

  with Result do
  begin
    AddField('dwStatus', System.TypeInfo(DWORD));
    AddField('pszStatus', 'PWideChar');
    AddField('dwSeverity', System.TypeInfo(DWORD));

    Complete;
  end;
end;

{-------------------------}
{ _MONITOR_INFO_1A import }
{-------------------------}

function SepiImport_MONITOR_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_MONITOR_INFO_1A', False, True);

  with Result do
  begin
    AddField('pName', 'PAnsiChar');

    Complete;
  end;
end;

{-------------------------}
{ _MONITOR_INFO_1W import }
{-------------------------}

function SepiImport_MONITOR_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_MONITOR_INFO_1W', False, True);

  with Result do
  begin
    AddField('pName', 'PWideChar');

    Complete;
  end;
end;

{-------------------------}
{ _MONITOR_INFO_2A import }
{-------------------------}

function SepiImport_MONITOR_INFO_2A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_MONITOR_INFO_2A', False, True);

  with Result do
  begin
    AddField('pName', 'PAnsiChar');
    AddField('pEnvironment', 'PAnsiChar');
    AddField('pDLLName', 'PAnsiChar');

    Complete;
  end;
end;

{-------------------------}
{ _MONITOR_INFO_2W import }
{-------------------------}

function SepiImport_MONITOR_INFO_2W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_MONITOR_INFO_2W', False, True);

  with Result do
  begin
    AddField('pName', 'PWideChar');
    AddField('pEnvironment', 'PWideChar');
    AddField('pDLLName', 'PWideChar');

    Complete;
  end;
end;

{---------------------------}
{ _DATATYPES_INFO_1A import }
{---------------------------}

function SepiImport_DATATYPES_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DATATYPES_INFO_1A', False, True);

  with Result do
  begin
    AddField('pName', 'PAnsiChar');

    Complete;
  end;
end;

{---------------------------}
{ _DATATYPES_INFO_1W import }
{---------------------------}

function SepiImport_DATATYPES_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_DATATYPES_INFO_1W', False, True);

  with Result do
  begin
    AddField('pName', 'PWideChar');

    Complete;
  end;
end;

{---------------------------}
{ _PRINTER_DEFAULTSA import }
{---------------------------}

function SepiImport_PRINTER_DEFAULTSA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_DEFAULTSA', False, True);

  with Result do
  begin
    AddField('pDatatype', 'PAnsiChar');
    AddField('pDevMode', 'PDeviceModeA');
    AddField('DesiredAccess', System.TypeInfo(ACCESS_MASK));

    Complete;
  end;
end;

{---------------------------}
{ _PRINTER_DEFAULTSW import }
{---------------------------}

function SepiImport_PRINTER_DEFAULTSW(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_DEFAULTSW', False, True);

  with Result do
  begin
    AddField('pDatatype', 'PWideChar');
    AddField('pDevMode', 'PDeviceModeW');
    AddField('DesiredAccess', System.TypeInfo(ACCESS_MASK));

    Complete;
  end;
end;

{-------------------------------------}
{ _PRINTER_NOTIFY_OPTIONS_TYPE import }
{-------------------------------------}

function SepiImport_PRINTER_NOTIFY_OPTIONS_TYPE(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_NOTIFY_OPTIONS_TYPE', False, True);

  with Result do
  begin
    AddField('wType', System.TypeInfo(Word));
    AddField('Reserved0', System.TypeInfo(Word));
    AddField('Reserved1', System.TypeInfo(DWORD));
    AddField('Reserved2', System.TypeInfo(DWORD));
    AddField('Count', System.TypeInfo(DWORD));
    AddField('pFields', 'PWord');

    Complete;
  end;
end;

{--------------------------------}
{ _PRINTER_NOTIFY_OPTIONS import }
{--------------------------------}

function SepiImport_PRINTER_NOTIFY_OPTIONS(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_NOTIFY_OPTIONS', False, True);

  with Result do
  begin
    AddField('Version', System.TypeInfo(DWORD));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('Count', System.TypeInfo(DWORD));
    AddField('pTypes', 'PPrinterNotifyOptionsType');

    Complete;
  end;
end;

{-----------}
{ $3 import }
{-----------}

function SepiImport_3(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '$3', False, True);

  with Result do
  begin
    AddField('cbBuf', System.TypeInfo(DWORD));
    AddField('pBuf', 'Pointer');

    Complete;
  end;
end;

{-----------}
{ $1 import }
{-----------}

function SepiImport_1(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '$1', False, True);

  with Result do
  begin
    AddFieldAfter('adwData', '$2', '');
    AddFieldAfter('Data', '$3', '');

    Complete;
  end;
end;

{----------------------------------}
{ _PRINTER_NOTIFY_INFO_DATA import }
{----------------------------------}

function SepiImport_PRINTER_NOTIFY_INFO_DATA(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_NOTIFY_INFO_DATA', False, True);

  with Result do
  begin
    AddField('wType', System.TypeInfo(Word));
    AddField('Field', System.TypeInfo(Word));
    AddField('Reserved', System.TypeInfo(DWORD));
    AddField('Id', System.TypeInfo(DWORD));
    AddField('NotifyData', '$1');

    Complete;
  end;
end;

{-----------------------------}
{ _PRINTER_NOTIFY_INFO import }
{-----------------------------}

function SepiImport_PRINTER_NOTIFY_INFO(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PRINTER_NOTIFY_INFO', False, True);

  with Result do
  begin
    AddField('Version', System.TypeInfo(DWORD));
    AddField('Flags', System.TypeInfo(DWORD));
    AddField('Count', System.TypeInfo(DWORD));
    AddField('aData', '$4');

    Complete;
  end;
end;

{--------------------------}
{ _PROVIDOR_INFO_1A import }
{--------------------------}

function SepiImport_PROVIDOR_INFO_1A(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PROVIDOR_INFO_1A', False, True);

  with Result do
  begin
    AddField('pName', 'PAnsiChar');
    AddField('pEnvironment', 'PAnsiChar');
    AddField('pDLLName', 'PAnsiChar');

    Complete;
  end;
end;

{--------------------------}
{ _PROVIDOR_INFO_1W import }
{--------------------------}

function SepiImport_PROVIDOR_INFO_1W(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, '_PROVIDOR_INFO_1W', False, True);

  with Result do
  begin
    AddField('pName', 'PWideChar');
    AddField('pEnvironment', 'PWideChar');
    AddField('pDLLName', 'PWideChar');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'WinSpoolTypes',
    ['Windows']);

  // Types
  TSepiPointerType.Create(Result, 'PPrinterInfo1A', 'TPrinterInfo1A', True);
  TSepiPointerType.Create(Result, 'PPrinterInfo1W', 'TPrinterInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PPrinterInfo1', 'PPrinterInfo1A');
  SepiImport_PRINTER_INFO_1A(Result);
  SepiImport_PRINTER_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_PRINTER_INFO_1', '_PRINTER_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo1A', '_PRINTER_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo1W', '_PRINTER_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo1', 'TPrinterInfo1A');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_1A', '_PRINTER_INFO_1A');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_1W', '_PRINTER_INFO_1W');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_1', 'PRINTER_INFO_1A');
  TSepiPointerType.Create(Result, 'PPrinterInfo2A', 'TPrinterInfo2A', True);
  TSepiPointerType.Create(Result, 'PPrinterInfo2W', 'TPrinterInfo2W', True);
  TSepiTypeAlias.Create(Result, 'PPrinterInfo2', 'PPrinterInfo2A');
  SepiImport_PRINTER_INFO_2A(Result);
  SepiImport_PRINTER_INFO_2W(Result);
  TSepiTypeAlias.Create(Result, '_PRINTER_INFO_2', '_PRINTER_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo2A', '_PRINTER_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo2W', '_PRINTER_INFO_2W');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo2', 'TPrinterInfo2A');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_2A', '_PRINTER_INFO_2A');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_2W', '_PRINTER_INFO_2W');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_2', 'PRINTER_INFO_2A');
  TSepiPointerType.Create(Result, 'PPrinterInfo3', 'TPrinterInfo3', True);
  SepiImport_PRINTER_INFO_3(Result);
  TSepiTypeAlias.Create(Result, 'TPrinterInfo3', '_PRINTER_INFO_3');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_3', '_PRINTER_INFO_3');
  TSepiPointerType.Create(Result, 'PPrinterInfo4A', 'TPrinterInfo4A', True);
  TSepiPointerType.Create(Result, 'PPrinterInfo4W', 'TPrinterInfo4W', True);
  TSepiTypeAlias.Create(Result, 'PPrinterInfo4', 'PPrinterInfo4A');
  SepiImport_PRINTER_INFO_4A(Result);
  SepiImport_PRINTER_INFO_4W(Result);
  TSepiTypeAlias.Create(Result, '_PRINTER_INFO_4', '_PRINTER_INFO_4A');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo4A', '_PRINTER_INFO_4A');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo4W', '_PRINTER_INFO_4W');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo4', 'TPrinterInfo4A');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_4A', '_PRINTER_INFO_4A');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_4W', '_PRINTER_INFO_4W');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_4', 'PRINTER_INFO_4A');
  TSepiPointerType.Create(Result, 'PPrinterInfo5A', 'TPrinterInfo5A', True);
  TSepiPointerType.Create(Result, 'PPrinterInfo5W', 'TPrinterInfo5W', True);
  TSepiTypeAlias.Create(Result, 'PPrinterInfo5', 'PPrinterInfo5A');
  SepiImport_PRINTER_INFO_5A(Result);
  SepiImport_PRINTER_INFO_5W(Result);
  TSepiTypeAlias.Create(Result, '_PRINTER_INFO_5', '_PRINTER_INFO_5A');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo5A', '_PRINTER_INFO_5A');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo5W', '_PRINTER_INFO_5W');
  TSepiTypeAlias.Create(Result, 'TPrinterInfo5', 'TPrinterInfo5A');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_5A', '_PRINTER_INFO_5A');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_5W', '_PRINTER_INFO_5W');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_5', 'PRINTER_INFO_5A');
  TSepiPointerType.Create(Result, 'PPrinterInfo6', 'TPrinterInfo6', True);
  SepiImport_PRINTER_INFO_6(Result);
  TSepiTypeAlias.Create(Result, 'TPrinterInfo6', '_PRINTER_INFO_6');
  TSepiTypeAlias.Create(Result, 'PRINTER_INFO_6', '_PRINTER_INFO_6');

  // Types
  TSepiPointerType.Create(Result, 'PJobInfo1A', 'TJobInfo1A', True);
  TSepiPointerType.Create(Result, 'PJobInfo1W', 'TJobInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PJobInfo1', 'PJobInfo1A');
  SepiImport_JOB_INFO_1A(Result);
  SepiImport_JOB_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_JOB_INFO_1', '_JOB_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TJobInfo1A', '_JOB_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TJobInfo1W', '_JOB_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TJobInfo1', 'TJobInfo1A');
  TSepiTypeAlias.Create(Result, 'JOB_INFO_1A', '_JOB_INFO_1A');
  TSepiTypeAlias.Create(Result, 'JOB_INFO_1W', '_JOB_INFO_1W');
  TSepiTypeAlias.Create(Result, 'JOB_INFO_1', 'JOB_INFO_1A');
  TSepiPointerType.Create(Result, 'PJobInfo2A', 'TJobInfo2A', True);
  TSepiPointerType.Create(Result, 'PJobInfo2W', 'TJobInfo2W', True);
  TSepiTypeAlias.Create(Result, 'PJobInfo2', 'PJobInfo2A');
  SepiImport_JOB_INFO_2A(Result);
  SepiImport_JOB_INFO_2W(Result);
  TSepiTypeAlias.Create(Result, '_JOB_INFO_2', '_JOB_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TJobInfo2A', '_JOB_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TJobInfo2W', '_JOB_INFO_2W');
  TSepiTypeAlias.Create(Result, 'TJobInfo2', 'TJobInfo2A');
  TSepiTypeAlias.Create(Result, 'JOB_INFO_2A', '_JOB_INFO_2A');
  TSepiTypeAlias.Create(Result, 'JOB_INFO_2W', '_JOB_INFO_2W');
  TSepiTypeAlias.Create(Result, 'JOB_INFO_2', 'JOB_INFO_2A');
  TSepiPointerType.Create(Result, 'PJobInfo3', 'TJobInfo3', True);
  SepiImport_JOB_INFO_3(Result);
  TSepiTypeAlias.Create(Result, 'TJobInfo3', '_JOB_INFO_3');
  TSepiTypeAlias.Create(Result, 'JOB_INFO_3', '_JOB_INFO_3');

  // Types
  TSepiPointerType.Create(Result, 'PAddJobInfo1A', 'TAddJobInfo1A', True);
  TSepiPointerType.Create(Result, 'PAddJobInfo1W', 'TAddJobInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PAddJobInfo1', 'PAddJobInfo1A');
  SepiImport_ADDJOB_INFO_1A(Result);
  SepiImport_ADDJOB_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_ADDJOB_INFO_1', '_ADDJOB_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TAddJobInfo1A', '_ADDJOB_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TAddJobInfo1W', '_ADDJOB_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TAddJobInfo1', 'TAddJobInfo1A');
  TSepiTypeAlias.Create(Result, 'ADDJOB_INFO_1A', '_ADDJOB_INFO_1A');
  TSepiTypeAlias.Create(Result, 'ADDJOB_INFO_1W', '_ADDJOB_INFO_1W');
  TSepiTypeAlias.Create(Result, 'ADDJOB_INFO_1', 'ADDJOB_INFO_1A');
  TSepiPointerType.Create(Result, 'PDriverInfo1A', 'TDriverInfo1A', True);
  TSepiPointerType.Create(Result, 'PDriverInfo1W', 'TDriverInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PDriverInfo1', 'PDriverInfo1A');
  SepiImport_DRIVER_INFO_1A(Result);
  SepiImport_DRIVER_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_DRIVER_INFO_1', '_DRIVER_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TDriverInfo1A', '_DRIVER_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TDriverInfo1W', '_DRIVER_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TDriverInfo1', 'TDriverInfo1A');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_1A', '_DRIVER_INFO_1A');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_1W', '_DRIVER_INFO_1W');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_1', 'DRIVER_INFO_1A');
  TSepiPointerType.Create(Result, 'PDriverInfo2A', 'TDriverInfo2A', True);
  TSepiPointerType.Create(Result, 'PDriverInfo2W', 'TDriverInfo2W', True);
  TSepiTypeAlias.Create(Result, 'PDriverInfo2', 'PDriverInfo2A');
  SepiImport_DRIVER_INFO_2A(Result);
  SepiImport_DRIVER_INFO_2W(Result);
  TSepiTypeAlias.Create(Result, '_DRIVER_INFO_2', '_DRIVER_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TDriverInfo2A', '_DRIVER_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TDriverInfo2W', '_DRIVER_INFO_2W');
  TSepiTypeAlias.Create(Result, 'TDriverInfo2', 'TDriverInfo2A');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_2A', '_DRIVER_INFO_2A');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_2W', '_DRIVER_INFO_2W');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_2', 'DRIVER_INFO_2A');
  TSepiPointerType.Create(Result, 'PDriverInfo3A', 'TDriverInfo3A', True);
  TSepiPointerType.Create(Result, 'PDriverInfo3W', 'TDriverInfo3W', True);
  TSepiTypeAlias.Create(Result, 'PDriverInfo3', 'PDriverInfo3A');
  SepiImport_DRIVER_INFO_3A(Result);
  SepiImport_DRIVER_INFO_3W(Result);
  TSepiTypeAlias.Create(Result, '_DRIVER_INFO_3', '_DRIVER_INFO_3A');
  TSepiTypeAlias.Create(Result, 'TDriverInfo3A', '_DRIVER_INFO_3A');
  TSepiTypeAlias.Create(Result, 'TDriverInfo3W', '_DRIVER_INFO_3W');
  TSepiTypeAlias.Create(Result, 'TDriverInfo3', 'TDriverInfo3A');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_3A', '_DRIVER_INFO_3A');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_3W', '_DRIVER_INFO_3W');
  TSepiTypeAlias.Create(Result, 'DRIVER_INFO_3', 'DRIVER_INFO_3A');
  TSepiPointerType.Create(Result, 'PDocInfo1A', 'TDocInfo1A', True);
  TSepiPointerType.Create(Result, 'PDocInfo1W', 'TDocInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PDocInfo1', 'PDocInfo1A');
  SepiImport_DOC_INFO_1A(Result);
  SepiImport_DOC_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_DOC_INFO_1', '_DOC_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TDocInfo1A', '_DOC_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TDocInfo1W', '_DOC_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TDocInfo1', 'TDocInfo1A');
  TSepiTypeAlias.Create(Result, 'DOC_INFO_1A', '_DOC_INFO_1A');
  TSepiTypeAlias.Create(Result, 'DOC_INFO_1W', '_DOC_INFO_1W');
  TSepiTypeAlias.Create(Result, 'DOC_INFO_1', 'DOC_INFO_1A');
  TSepiPointerType.Create(Result, 'PFormInfo1A', 'TFormInfo1A', True);
  TSepiPointerType.Create(Result, 'PFormInfo1W', 'TFormInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PFormInfo1', 'PFormInfo1A');
  SepiImport_FORM_INFO_1A(Result);
  SepiImport_FORM_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_FORM_INFO_1', '_FORM_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TFormInfo1A', '_FORM_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TFormInfo1W', '_FORM_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TFormInfo1', 'TFormInfo1A');
  TSepiTypeAlias.Create(Result, 'FORM_INFO_1A', '_FORM_INFO_1A');
  TSepiTypeAlias.Create(Result, 'FORM_INFO_1W', '_FORM_INFO_1W');
  TSepiTypeAlias.Create(Result, 'FORM_INFO_1', 'FORM_INFO_1A');
  TSepiPointerType.Create(Result, 'PDocInfo2A', 'TDocInfo2A', True);
  TSepiPointerType.Create(Result, 'PDocInfo2W', 'TDocInfo2W', True);
  TSepiTypeAlias.Create(Result, 'PDocInfo2', 'PDocInfo2A');
  SepiImport_DOC_INFO_2A(Result);
  SepiImport_DOC_INFO_2W(Result);
  TSepiTypeAlias.Create(Result, '_DOC_INFO_2', '_DOC_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TDocInfo2A', '_DOC_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TDocInfo2W', '_DOC_INFO_2W');
  TSepiTypeAlias.Create(Result, 'TDocInfo2', 'TDocInfo2A');
  TSepiTypeAlias.Create(Result, 'DOC_INFO_2A', '_DOC_INFO_2A');
  TSepiTypeAlias.Create(Result, 'DOC_INFO_2W', '_DOC_INFO_2W');
  TSepiTypeAlias.Create(Result, 'DOC_INFO_2', 'DOC_INFO_2A');

  // Types
  TSepiPointerType.Create(Result, 'PPrintProcessorInfo1A', 'TPrintProcessorInfo1A', True);
  TSepiPointerType.Create(Result, 'PPrintProcessorInfo1W', 'TPrintProcessorInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PPrintProcessorInfo1', 'PPrintProcessorInfo1A');
  SepiImport_PRINTPROCESSOR_INFO_1A(Result);
  SepiImport_PRINTPROCESSOR_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_PRINTPROCESSOR_INFO_1', '_PRINTPROCESSOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TPrintProcessorInfo1A', '_PRINTPROCESSOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TPrintProcessorInfo1W', '_PRINTPROCESSOR_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TPrintProcessorInfo1', 'TPrintProcessorInfo1A');
  TSepiTypeAlias.Create(Result, 'PRINTPROCESSOR_INFO_1A', '_PRINTPROCESSOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'PRINTPROCESSOR_INFO_1W', '_PRINTPROCESSOR_INFO_1W');
  TSepiTypeAlias.Create(Result, 'PRINTPROCESSOR_INFO_1', 'PRINTPROCESSOR_INFO_1A');
  TSepiPointerType.Create(Result, 'PPortInfo1A', 'TPortInfo1A', True);
  TSepiPointerType.Create(Result, 'PPortInfo1W', 'TPortInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PPortInfo1', 'PPortInfo1A');
  SepiImport_PORT_INFO_1A(Result);
  SepiImport_PORT_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_PORT_INFO_1', '_PORT_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TPortInfo1A', '_PORT_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TPortInfo1W', '_PORT_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TPortInfo1', 'TPortInfo1A');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_1A', '_PORT_INFO_1A');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_1W', '_PORT_INFO_1W');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_1', 'PORT_INFO_1A');
  TSepiPointerType.Create(Result, 'PPortInfo2A', 'TPortInfo2A', True);
  TSepiPointerType.Create(Result, 'PPortInfo2W', 'TPortInfo2W', True);
  TSepiTypeAlias.Create(Result, 'PPortInfo2', 'PPortInfo2A');
  SepiImport_PORT_INFO_2A(Result);
  SepiImport_PORT_INFO_2W(Result);
  TSepiTypeAlias.Create(Result, '_PORT_INFO_2', '_PORT_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TPortInfo2A', '_PORT_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TPortInfo2W', '_PORT_INFO_2W');
  TSepiTypeAlias.Create(Result, 'TPortInfo2', 'TPortInfo2A');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_2A', '_PORT_INFO_2A');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_2W', '_PORT_INFO_2W');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_2', 'PORT_INFO_2A');

  // Types
  TSepiPointerType.Create(Result, 'PPortInfo3A', 'TPortInfo3A', True);
  TSepiPointerType.Create(Result, 'PPortInfo3W', 'TPortInfo3W', True);
  TSepiTypeAlias.Create(Result, 'PPortInfo3', 'PPortInfo3A');
  SepiImport_PORT_INFO_3A(Result);
  SepiImport_PORT_INFO_3W(Result);
  TSepiTypeAlias.Create(Result, '_PORT_INFO_3', '_PORT_INFO_3A');
  TSepiTypeAlias.Create(Result, 'TPortInfo3A', '_PORT_INFO_3A');
  TSepiTypeAlias.Create(Result, 'TPortInfo3W', '_PORT_INFO_3W');
  TSepiTypeAlias.Create(Result, 'TPortInfo3', 'TPortInfo3A');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_3A', '_PORT_INFO_3A');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_3W', '_PORT_INFO_3W');
  TSepiTypeAlias.Create(Result, 'PORT_INFO_3', 'PORT_INFO_3A');

  // Types
  TSepiPointerType.Create(Result, 'PMonitorInfo1A', 'TMonitorInfo1A', True);
  TSepiPointerType.Create(Result, 'PMonitorInfo1W', 'TMonitorInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PMonitorInfo1', 'PMonitorInfo1A');
  SepiImport_MONITOR_INFO_1A(Result);
  SepiImport_MONITOR_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_MONITOR_INFO_1', '_MONITOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TMonitorInfo1A', '_MONITOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TMonitorInfo1W', '_MONITOR_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TMonitorInfo1', 'TMonitorInfo1A');
  TSepiTypeAlias.Create(Result, 'MONITOR_INFO_1A', '_MONITOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'MONITOR_INFO_1W', '_MONITOR_INFO_1W');
  TSepiTypeAlias.Create(Result, 'MONITOR_INFO_1', 'MONITOR_INFO_1A');
  TSepiPointerType.Create(Result, 'PMonitorInfo2A', 'TMonitorInfo2A', True);
  TSepiPointerType.Create(Result, 'PMonitorInfo2W', 'TMonitorInfo2W', True);
  TSepiTypeAlias.Create(Result, 'PMonitorInfo2', 'PMonitorInfo2A');
  SepiImport_MONITOR_INFO_2A(Result);
  SepiImport_MONITOR_INFO_2W(Result);
  TSepiTypeAlias.Create(Result, '_MONITOR_INFO_2', '_MONITOR_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TMonitorInfo2A', '_MONITOR_INFO_2A');
  TSepiTypeAlias.Create(Result, 'TMonitorInfo2W', '_MONITOR_INFO_2W');
  TSepiTypeAlias.Create(Result, 'TMonitorInfo2', 'TMonitorInfo2A');
  TSepiTypeAlias.Create(Result, 'MONITOR_INFO_2A', '_MONITOR_INFO_2A');
  TSepiTypeAlias.Create(Result, 'MONITOR_INFO_2W', '_MONITOR_INFO_2W');
  TSepiTypeAlias.Create(Result, 'MONITOR_INFO_2', 'MONITOR_INFO_2A');
  TSepiPointerType.Create(Result, 'PDatatypesInfo1A', 'TDatatypesInfo1A', True);
  TSepiPointerType.Create(Result, 'PDatatypesInfo1W', 'TDatatypesInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PDatatypesInfo1', 'PDatatypesInfo1A');
  SepiImport_DATATYPES_INFO_1A(Result);
  SepiImport_DATATYPES_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_DATATYPES_INFO_1', '_DATATYPES_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TDatatypesInfo1A', '_DATATYPES_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TDatatypesInfo1W', '_DATATYPES_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TDatatypesInfo1', 'TDatatypesInfo1A');
  TSepiTypeAlias.Create(Result, 'DATATYPES_INFO_1A', '_DATATYPES_INFO_1A');
  TSepiTypeAlias.Create(Result, 'DATATYPES_INFO_1W', '_DATATYPES_INFO_1W');
  TSepiTypeAlias.Create(Result, 'DATATYPES_INFO_1', 'DATATYPES_INFO_1A');
  TSepiPointerType.Create(Result, 'PPrinterDefaultsA', 'TPrinterDefaultsA', True);
  TSepiPointerType.Create(Result, 'PPrinterDefaultsW', 'TPrinterDefaultsW', True);
  TSepiTypeAlias.Create(Result, 'PPrinterDefaults', 'PPrinterDefaultsA');
  SepiImport_PRINTER_DEFAULTSA(Result);
  SepiImport_PRINTER_DEFAULTSW(Result);
  TSepiTypeAlias.Create(Result, '_PRINTER_DEFAULTS', '_PRINTER_DEFAULTSA');
  TSepiTypeAlias.Create(Result, 'TPrinterDefaultsA', '_PRINTER_DEFAULTSA');
  TSepiTypeAlias.Create(Result, 'TPrinterDefaultsW', '_PRINTER_DEFAULTSW');
  TSepiTypeAlias.Create(Result, 'TPrinterDefaults', 'TPrinterDefaultsA');
  TSepiTypeAlias.Create(Result, 'PRINTER_DEFAULTSA', '_PRINTER_DEFAULTSA');
  TSepiTypeAlias.Create(Result, 'PRINTER_DEFAULTSW', '_PRINTER_DEFAULTSW');
  TSepiTypeAlias.Create(Result, 'PRINTER_DEFAULTS', 'PRINTER_DEFAULTSA');

  // Types
  TSepiPointerType.Create(Result, 'PPrinterNotifyOptionsType', 'TPrinterNotifyOptionsType', True);
  SepiImport_PRINTER_NOTIFY_OPTIONS_TYPE(Result);
  TSepiTypeAlias.Create(Result, 'TPrinterNotifyOptionsType', '_PRINTER_NOTIFY_OPTIONS_TYPE');
  TSepiTypeAlias.Create(Result, 'PRINTER_NOTIFY_OPTIONS_TYPE', '_PRINTER_NOTIFY_OPTIONS_TYPE');

  // Types
  TSepiPointerType.Create(Result, 'PPrinterNotifyOptions', 'TPrinterNotifyOptions', True);
  SepiImport_PRINTER_NOTIFY_OPTIONS(Result);
  TSepiTypeAlias.Create(Result, 'TPrinterNotifyOptions', '_PRINTER_NOTIFY_OPTIONS');
  TSepiTypeAlias.Create(Result, 'PRINTER_NOTIFY_OPTIONS', '_PRINTER_NOTIFY_OPTIONS');

  // Types
  TSepiPointerType.Create(Result, 'PPrinterNotifyInfoData', 'TPrinterNotifyInfoData', True);
  TSepiArrayType.Create(Result, '$2',
    [0, 1], TypeInfo(DWORD), True);
  SepiImport_3(Result);
  SepiImport_1(Result);
  SepiImport_PRINTER_NOTIFY_INFO_DATA(Result);
  TSepiTypeAlias.Create(Result, 'TPrinterNotifyInfoData', '_PRINTER_NOTIFY_INFO_DATA');
  TSepiTypeAlias.Create(Result, 'PRINTER_NOTIFY_INFO_DATA', '_PRINTER_NOTIFY_INFO_DATA');
  TSepiPointerType.Create(Result, 'PPrinterNotifyInfo', 'TPrinterNotifyInfo', True);
  TSepiArrayType.Create(Result, '$4',
    [0, 0], 'TPrinterNotifyInfoData', True);
  SepiImport_PRINTER_NOTIFY_INFO(Result);
  TSepiTypeAlias.Create(Result, 'TPrinterNotifyInfo', '_PRINTER_NOTIFY_INFO');
  TSepiTypeAlias.Create(Result, 'PRINTER_NOTIFY_INFO', '_PRINTER_NOTIFY_INFO');

  // Types
  TSepiPointerType.Create(Result, 'PProvidorInfo1A', 'TProvidorInfo1A', True);
  TSepiPointerType.Create(Result, 'PProvidorInfo1W', 'TProvidorInfo1W', True);
  TSepiTypeAlias.Create(Result, 'PProvidorInfo1', 'PProvidorInfo1A');
  SepiImport_PROVIDOR_INFO_1A(Result);
  SepiImport_PROVIDOR_INFO_1W(Result);
  TSepiTypeAlias.Create(Result, '_PROVIDOR_INFO_1', '_PROVIDOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TProvidorInfo1A', '_PROVIDOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'TProvidorInfo1W', '_PROVIDOR_INFO_1W');
  TSepiTypeAlias.Create(Result, 'TProvidorInfo1', 'TProvidorInfo1A');
  TSepiTypeAlias.Create(Result, 'PROVIDOR_INFO_1A', '_PROVIDOR_INFO_1A');
  TSepiTypeAlias.Create(Result, 'PROVIDOR_INFO_1W', '_PROVIDOR_INFO_1W');
  TSepiTypeAlias.Create(Result, 'PROVIDOR_INFO_1', 'PROVIDOR_INFO_1A');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('WinSpoolTypes', ImportUnit);
end.

