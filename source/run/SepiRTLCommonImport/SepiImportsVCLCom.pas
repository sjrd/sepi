{*
  Importe l'unité VCLCom dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsVCLCom;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, VCLCom, ActiveX;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTComponentFactory = class(TComponentFactory)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{--------------------------}
{ TComponentFactory import }
{--------------------------}

class function TSepiImportsTComponentFactory.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TComponentFactory));

  with Result do
  begin
    AddInterface(System.TypeInfo(IClassFactory));

    CurrentVisibility := mvProtected;

    AddMethod('CreateInstance', @TSepiImportsTComponentFactory.CreateInstance,
      'function(const UnkOuter: IUnknown; const IID: TGUID; out Obj ) : HResult',
      ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTComponentFactory.Create,
      'constructor(ComServer: TComServerObject; ComponentClass: TComponentClass ; const ClassID: TGUID ; Instancing: TClassInstancing ; ThreadingModel: TThreadingModel = tmSingle )');
    AddMethod('CreateComObject',
      @TSepiImportsTComponentFactory.CreateComObject,
      'function(const Controller: IUnknown): TComObject',
      mlkOverride);
    AddMethod('UpdateRegistry', @TSepiImportsTComponentFactory.UpdateRegistry,
      'procedure(Register: Boolean)',
      mlkOverride);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'VCLCom',
    ['ActiveXTypes', 'ComObj', 'Classes']);

  // Types
  TSepiImportsTComponentFactory.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('VCLCom', ImportUnit);
end.

