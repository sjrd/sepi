{*
  Importe l'unité Clipbrd dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsClipbrd;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Windows, Clipbrd;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTClipboard = class(TClipboard)
  private
    function GetAsText: string;
    function GetClipboardWindow: HWND;
    function GetFormatCount: Integer;
    function GetFormats(Index: Integer): Word;
    procedure SetAsText(const Value: string);
    class function SepiImport(Owner : TSepiUnit) : TSepiClass;
  end;

{-------------------}
{ TClipboard import }
{-------------------}

function TSepiImportsTClipboard.GetAsText: string;
begin
  Result := AsText;
end;

function TSepiImportsTClipboard.GetClipboardWindow: HWND;
begin
  Result := Handle;
end;

function TSepiImportsTClipboard.GetFormatCount: Integer;
begin
  Result := FormatCount;
end;

function TSepiImportsTClipboard.GetFormats(Index: Integer): Word;
begin
  Result := Formats[Index];
end;

procedure TSepiImportsTClipboard.SetAsText(const Value: string);
begin
  AsText := Value;
end;

class function TSepiImportsTClipboard.SepiImport(
  Owner : TSepiUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TClipboard));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FOpenRefCount', System.TypeInfo(Integer));
    AddField('FClipboardWindow', System.TypeInfo(HWND));
    AddField('FAllocated', System.TypeInfo(Boolean));
    AddField('FEmptied', System.TypeInfo(Boolean));

    AddMethod('Adding', nil,
      'procedure');
    AddMethod('AssignGraphic', nil,
      'procedure(Source: TGraphic)');
    AddMethod('AssignPicture', nil,
      'procedure(Source: TPicture)');
    AddMethod('AssignToBitmap', nil,
      'procedure(Dest: TBitmap)');
    AddMethod('AssignToMetafile', nil,
      'procedure(Dest: TMetafile)');
    AddMethod('AssignToPicture', nil,
      'procedure(Dest: TPicture)');
    AddMethod('GetAsText', @TSepiImportsTClipboard.GetAsText,
      'function: string');
    AddMethod('GetClipboardWindow', @TSepiImportsTClipboard.GetClipboardWindow,
      'function: HWND');
    AddMethod('GetFormatCount', @TSepiImportsTClipboard.GetFormatCount,
      'function: Integer');
    AddMethod('GetFormats', @TSepiImportsTClipboard.GetFormats,
      'function(Index: Integer): Word');
    AddMethod('SetAsText', @TSepiImportsTClipboard.SetAsText,
      'procedure(const Value: string)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTClipboard.AssignTo,
      'procedure(Dest: TPersistent)',
      mlkOverride);
    AddMethod('SetBuffer', @TSepiImportsTClipboard.SetBuffer,
      'procedure(Format: Word; var Buffer; Size: Integer)');
    AddMethod('WndProc', @TSepiImportsTClipboard.WndProc,
      'procedure(var Message: TMessage)',
      mlkVirtual);
    AddMethod('MainWndProc', @TSepiImportsTClipboard.MainWndProc,
      'procedure(var Message: TMessage)');

    AddProperty('Handle', 'property: HWND',
      'GetClipboardWindow', '');
    AddProperty('OpenRefCount', 'property: Integer',
      'FOpenRefCount', '');

    CurrentVisibility := mvPublic;

    AddMethod('Destroy', @TSepiImportsTClipboard.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Assign', @TSepiImportsTClipboard.Assign,
      'procedure(Source: TPersistent)',
      mlkOverride);
    AddMethod('Clear', @TSepiImportsTClipboard.Clear,
      'procedure',
      mlkVirtual);
    AddMethod('Close', @TSepiImportsTClipboard.Close,
      'procedure',
      mlkVirtual);
    AddMethod('GetComponent', @TSepiImportsTClipboard.GetComponent,
      'function(Owner, Parent: TComponent): TComponent');
    AddMethod('GetAsHandle', @TSepiImportsTClipboard.GetAsHandle,
      'function(Format: Word): THandle');
    AddMethod('GetTextBuf', @TSepiImportsTClipboard.GetTextBuf,
      'function(Buffer: PChar; BufSize: Integer): Integer');
    AddMethod('HasFormat', @TSepiImportsTClipboard.HasFormat,
      'function(Format: Word): Boolean');
    AddMethod('Open', @TSepiImportsTClipboard.Open,
      'procedure',
      mlkVirtual);
    AddMethod('SetComponent', @TSepiImportsTClipboard.SetComponent,
      'procedure(Component: TComponent)');
    AddMethod('SetAsHandle', @TSepiImportsTClipboard.SetAsHandle,
      'procedure(Format: Word; Value: THandle)');
    AddMethod('SetTextBuf', @TSepiImportsTClipboard.SetTextBuf,
      'procedure(Buffer: PChar)');

    AddProperty('AsText', 'property: string',
      'GetAsText', 'SetAsText');
    AddProperty('FormatCount', 'property: Integer',
      'GetFormatCount', '');
    AddProperty('Formats', 'property[Index: Integer]: Word',
      'GetFormats', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiRoot) : TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'Clipbrd',
    ['Windows', 'Messages', 'Classes', 'Graphics']);

  // Global variables
  TSepiVariable.Create(Result, 'CF_PICTURE',
     CF_PICTURE, TypeInfo(Word));
  TSepiVariable.Create(Result, 'CF_COMPONENT',
     CF_COMPONENT, TypeInfo(Word));

  // Types
  TSepiImportsTClipboard.SepiImport(Result);

  // Routines
  TSepiMethod.Create(Result, 'Clipboard', @Clipboard,
    'function: TClipboard');
  TSepiMethod.Create(Result, 'SetClipboard', @SetClipboard,
    'function(NewClipboard: TClipboard): TClipboard');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Clipbrd', ImportUnit);
end.

