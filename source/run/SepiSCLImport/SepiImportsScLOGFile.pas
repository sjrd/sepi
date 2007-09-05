{*
  Importe l'unité ScLOGFile dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsScLOGFile;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, ScLOGFile, Classes;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTScLogFile = class(TScLogFile)
  private
    procedure Add_0(const Time, Title, Description : string);
    procedure Add_1(Time : TDateTime; const Title : string; const Description : string = '' );
    procedure Add_2(const Title : string; const Description : string = '' );
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-------------------}
{ TScLogFile import }
{-------------------}

procedure TSepiImportsTScLogFile.Add_0(const Time, Title, Description : string);
begin
  Add(Time, Title, Description);
end;

procedure TSepiImportsTScLogFile.Add_1(Time : TDateTime; const Title : string; const Description : string = '' );
begin
  Add(Time, Title, Description);
end;

procedure TSepiImportsTScLogFile.Add_2(const Title : string; const Description : string = '' );
begin
  Add(Title, Description);
end;

class function TSepiImportsTScLogFile.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScLogFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFile', System.TypeInfo(TFileStream));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScLogFile.Create,
      'constructor(const FileName : string; Append : boolean = False)');
    AddMethod('Destroy', @TSepiImportsTScLogFile.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AddLine', @TSepiImportsTScLogFile.AddLine,
      'procedure(const Line : string)');
    AddOverloadedMethod('Add', @TSepiImportsTScLogFile.Add_0,
      'procedure(const Time, Title, Description : string)');
    AddOverloadedMethod('Add', @TSepiImportsTScLogFile.Add_1,
      'procedure(Time : TDateTime; const Title : string; const Description : string = '''' )');
    AddOverloadedMethod('Add', @TSepiImportsTScLogFile.Add_2,
      'procedure(const Title : string; const Description : string = '''' )');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScLOGFile',
    ['Classes']);

  // Types
  TSepiImportsTScLogFile.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScLOGFile', ImportUnit);
end.

