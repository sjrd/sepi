{*
  Importe l'unité SvImages dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsSvImages;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, Controls, ExtCtrls, SvImages;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTSvDropImage = class(TSvDropImage)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{---------------------}
{ TSvDropImage import }
{---------------------}

class function TSepiImportsTSvDropImage.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TSvDropImage));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('ImageBis', System.TypeInfo(TImage));
    AddField('FDropControl', System.TypeInfo(TControl));
    AddField('FOnDrop', System.TypeInfo(TDropImageEvent));

    AddMethod('MoveBisAt', nil,
      'procedure(X, Y : integer)');

    CurrentVisibility := mvProtected;

    AddMethod('MouseDown', @TSepiImportsTSvDropImage.MouseDown,
      'procedure(Button : TMouseButton; Shift : TShiftState; X, Y : integer )',
      mlkOverride);
    AddMethod('MouseMove', @TSepiImportsTSvDropImage.MouseMove,
      'procedure(Shift : TShiftState; X, Y : integer)',
      mlkOverride);
    AddMethod('MouseUp', @TSepiImportsTSvDropImage.MouseUp,
      'procedure(Button : TMouseButton; Shift : TShiftState; X, Y : integer )',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTSvDropImage.Create,
      'constructor(AOwner : TComponent)',
      mlkOverride);

    CurrentVisibility := mvPublished;

    AddProperty('DropControl', 'property: TControl',
      'FDropControl', 'FDropControl',
      NoIndex, Integer(nil));
    AddProperty('OnDrop', 'property: TDropImageEvent',
      'FOnDrop', 'FOnDrop',
      NoIndex, Integer(nil));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'SvImages',
    ['Windows', 'Forms', 'Classes', 'Controls', 'ExtCtrls']);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDropImageEvent));
  TSepiImportsTSvDropImage.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('SvImages', ImportUnit);
end.

