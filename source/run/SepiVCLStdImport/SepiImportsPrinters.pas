{*
  Importe l'unité Printers dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsPrinters;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Windows, Classes, Graphics, Printers;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEPrinter = class(EPrinter)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPrinter = class(TPrinter)
  private
    function GetCanvas: TCanvas;
    function GetNumCopies: Integer;
    function GetFonts: TStrings;
    function GetHandle: HDC;
    function GetOrientation: TPrinterOrientation;
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    function GetPrinterIndex: Integer;
    procedure SetPrinterIndex(Value: Integer);
    function GetPrinters: TStrings;
    procedure SetNumCopies(Value: Integer);
    procedure SetOrientation(Value: TPrinterOrientation);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-----------------}
{ EPrinter import }
{-----------------}

class function TSepiImportsEPrinter.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EPrinter));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------}
{ TPrinter import }
{-----------------}

function TSepiImportsTPrinter.GetCanvas: TCanvas;
begin
  Result := Canvas;
end;

function TSepiImportsTPrinter.GetNumCopies: Integer;
begin
  Result := Copies;
end;

function TSepiImportsTPrinter.GetFonts: TStrings;
begin
  Result := Fonts;
end;

function TSepiImportsTPrinter.GetHandle: HDC;
begin
  Result := Handle;
end;

function TSepiImportsTPrinter.GetOrientation: TPrinterOrientation;
begin
  Result := Orientation;
end;

function TSepiImportsTPrinter.GetPageHeight: Integer;
begin
  Result := PageHeight;
end;

function TSepiImportsTPrinter.GetPageWidth: Integer;
begin
  Result := PageWidth;
end;

function TSepiImportsTPrinter.GetPrinterIndex: Integer;
begin
  Result := PrinterIndex;
end;

procedure TSepiImportsTPrinter.SetPrinterIndex(Value: Integer);
begin
  PrinterIndex := Value;
end;

function TSepiImportsTPrinter.GetPrinters: TStrings;
begin
  Result := Printers;
end;

procedure TSepiImportsTPrinter.SetNumCopies(Value: Integer);
begin
  Copies := Value;
end;

procedure TSepiImportsTPrinter.SetOrientation(Value: TPrinterOrientation);
begin
  Orientation := Value;
end;

class function TSepiImportsTPrinter.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPrinter));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCanvas', System.TypeInfo(TCanvas));
    AddField('FFonts', System.TypeInfo(TStrings));
    AddField('FPageNumber', System.TypeInfo(Integer));
    AddField('FPrinters', System.TypeInfo(TStrings));
    AddField('FPrinterIndex', System.TypeInfo(Integer));
    AddField('FTitle', System.TypeInfo(string));
    AddField('FPrinting', System.TypeInfo(Boolean));
    AddField('FAborted', System.TypeInfo(Boolean));
    AddField('FCapabilities', System.TypeInfo(TPrinterCapabilities));
    AddField('State', System.TypeInfo(TPrinterState));
    AddField('DC', System.TypeInfo(HDC));
    AddField('DevMode', 'PDeviceMode');
    AddField('DeviceMode', System.TypeInfo(THandle));
    AddField('FPrinterHandle', System.TypeInfo(THandle));

    AddMethod('SetState', nil,
      'procedure(Value: TPrinterState)');
    AddMethod('GetCanvas', @TSepiImportsTPrinter.GetCanvas,
      'function: TCanvas');
    AddMethod('GetNumCopies', @TSepiImportsTPrinter.GetNumCopies,
      'function: Integer');
    AddMethod('GetFonts', @TSepiImportsTPrinter.GetFonts,
      'function: TStrings');
    AddMethod('GetHandle', @TSepiImportsTPrinter.GetHandle,
      'function: HDC');
    AddMethod('GetOrientation', @TSepiImportsTPrinter.GetOrientation,
      'function: TPrinterOrientation');
    AddMethod('GetPageHeight', @TSepiImportsTPrinter.GetPageHeight,
      'function: Integer');
    AddMethod('GetPageWidth', @TSepiImportsTPrinter.GetPageWidth,
      'function: Integer');
    AddMethod('GetPrinterIndex', @TSepiImportsTPrinter.GetPrinterIndex,
      'function: Integer');
    AddMethod('SetPrinterCapabilities', nil,
      'procedure(Value: Integer)');
    AddMethod('SetPrinterIndex', @TSepiImportsTPrinter.SetPrinterIndex,
      'procedure(Value: Integer)');
    AddMethod('GetPrinters', @TSepiImportsTPrinter.GetPrinters,
      'function: TStrings');
    AddMethod('SetNumCopies', @TSepiImportsTPrinter.SetNumCopies,
      'procedure(Value: Integer)');
    AddMethod('SetOrientation', @TSepiImportsTPrinter.SetOrientation,
      'procedure(Value: TPrinterOrientation)');
    AddMethod('SetToDefaultPrinter', nil,
      'procedure');
    AddMethod('CheckPrinting', nil,
      'procedure(Value: Boolean)');
    AddMethod('FreePrinters', nil,
      'procedure');
    AddMethod('FreeFonts', nil,
      'procedure');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPrinter.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTPrinter.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Abort', @TSepiImportsTPrinter.Abort,
      'procedure');
    AddMethod('BeginDoc', @TSepiImportsTPrinter.BeginDoc,
      'procedure');
    AddMethod('EndDoc', @TSepiImportsTPrinter.EndDoc,
      'procedure');
    AddMethod('NewPage', @TSepiImportsTPrinter.NewPage,
      'procedure');
    AddMethod('GetPrinter', @TSepiImportsTPrinter.GetPrinter,
      'procedure(ADevice, ADriver, APort: PChar; var ADeviceMode: THandle)');
    AddMethod('SetPrinter', @TSepiImportsTPrinter.SetPrinter,
      'procedure(ADevice, ADriver, APort: PChar; ADeviceMode: THandle)');
    AddMethod('Refresh', @TSepiImportsTPrinter.Refresh,
      'procedure');

    AddProperty('Aborted', 'property: Boolean',
      'FAborted', '');
    AddProperty('Canvas', 'property: TCanvas',
      'GetCanvas', '');
    AddProperty('Capabilities', 'property: TPrinterCapabilities',
      'FCapabilities', '');
    AddProperty('Copies', 'property: Integer',
      'GetNumCopies', 'SetNumCopies');
    AddProperty('Fonts', 'property: TStrings',
      'GetFonts', '');
    AddProperty('Handle', 'property: HDC',
      'GetHandle', '');
    AddProperty('Orientation', 'property: TPrinterOrientation',
      'GetOrientation', 'SetOrientation');
    AddProperty('PageHeight', 'property: Integer',
      'GetPageHeight', '');
    AddProperty('PageWidth', 'property: Integer',
      'GetPageWidth', '');
    AddProperty('PageNumber', 'property: Integer',
      'FPageNumber', '');
    AddProperty('PrinterIndex', 'property: Integer',
      'GetPrinterIndex', 'SetPrinterIndex');
    AddProperty('Printing', 'property: Boolean',
      'FPrinting', '');
    AddProperty('Printers', 'property: TStrings',
      'GetPrinters', '');
    AddProperty('Title', 'property: string',
      'FTitle', 'FTitle');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Printers',
    ['Windows', 'WinSpoolTypes', 'SysUtils', 'Classes', 'Graphics', 'Forms']);

  // Types
  TSepiImportsEPrinter.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrinterState));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrinterOrientation));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrinterCapability));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPrinterCapabilities));
  TSepiImportsTPrinter.SepiImport(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'Printer', @Printer,
    'function: TPrinter');
  TSepiMetaMethod.Create(Result, 'SetPrinter', @SetPrinter,
    'function(NewPrinter: TPrinter): TPrinter');
  {TSepiMetaMethod.Create(Result, 'AssignPrn', @AssignPrn,
    'procedure(var F: Text)');}

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Printers', ImportUnit);
end.

