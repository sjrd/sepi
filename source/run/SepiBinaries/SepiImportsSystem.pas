{*
  Importe l'unité System dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsSystem;

interface

uses
  SepiMetaUnits, SepiTypes;

type
  TSepiTObject = class(TObject)
  private
    class function ImportTObject(AOwner : TSepiMeta) : TSepiObjectType;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{ You must not localize any of the strings this unit contains! }

{----------------}
{ TObject import }
{----------------}

class function TSepiTObject.ImportTObject(AOwner : TSepiMeta) : TSepiObjectType;
var TObjectClass : TSepiClassType;
begin
  Result := TSepiObjectType.Create(AOwner, 'TObject', nil, TSepiTObject);
  TObjectClass := Result.ObjClassType;

  { Public declarations }
  Result.CurrentVisibility := mvPublic;

  TSepiMetaDelphiMethod.Create(TObjectClass, 'Create', @TObject.Create,
    mkConstructor);

  TSepiMetaDelphiMethod.Create(Result, 'Destroy', @TObject.Destroy,
    mkDestructor, osVirtual);

  TSepiMetaDelphiMethod.Create(Result, 'AfterConstruction',
    @TObject.AfterConstruction, mkNormal, osVirtual);

  TSepiMetaDelphiMethod.Create(Result, 'BeforeDestruction',
    @TObject.BeforeDestruction, mkNormal, osVirtual);

  TSepiMetaDelphiMethod.Create(Result, 'Free', @TObject.Free);

  { TClass }
  TSepiTypeAlias.Create(Result, 'TClass', TObjectClass);
end;

destructor TSepiTObject.Destroy;
begin
  inherited;
end;

procedure TSepiTObject.AfterConstruction;
begin
  inherited;
end;

procedure TSepiTObject.BeforeDestruction;
begin
  inherited;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
var I : integer;

    IntegerType : TSepiIntegerType;
    PointerType : TSepiIntegerType;
    DoubleType : TSepiDoubleType;
    StringType : TSepiStringType;
    ShortStringType : TSepiStringType;
    PCharType : TSepiStringType;
    BooleanType : TSepiBooleanType;

    HRESULTType : TSepiIntegerType;

    RecordType : TSepiRecordType;

    ObjectType : TSepiObjectType;
    ClassType : TSepiClassType;

    TObjectClass : TSepiObjectType;
begin
  Result := TSepiMetaUnit.Create(Root, 'System');

  { Base types }
  IntegerType     := TSepiIntegerType.Create(Result, 'integer');
  PointerType     := TSepiIntegerType.Create(Result, 'Pointer');
  DoubleType      := TSepiDoubleType .Create(Result, 'double');
  StringType      := TSepiStringType .Create(Result, 'string');
  ShortStringType := TSepiStringType .Create(Result, 'ShortString',
    ssShortString);
  PCharType       := TSepiStringType .Create(Result, 'PChar', ssPChar);
  BooleanType     := TSepiBooleanType.Create(Result, 'boolean');

  { Types declared in System.pas }
  HRESULTType := TSepiIntegerType.Create(Result, 'HRESULT');
  TSepiDoubleType.Create(Result, 'TDateTime');
  TSepiIntegerType.Create(Result, 'THandle');

  { TGUID record }
  RecordType := TSepiRecordType.Create(Result, 'TGUID');
  for I := 1 to 4 do
    TSepiMetaVariable.Create(RecordType, 'D'+Chr(I+48), IntegerType);

  { TMethod record }
  RecordType := TSepiRecordType.Create(Result, 'TMethod');
  TSepiMetaVariable.Create(RecordType, 'Code', PointerType);
  TSepiMetaVariable.Create(RecordType, 'Data', PointerType);

  { TObject class }
  TObjectClass := TSepiTObject.ImportTObject(Result);

  { Result constants }
  I := S_OK;
  TSepiConstant.Create(Result, 'S_OK', IntegerType).Value.WriteBuffer(I, 4);
  I := S_FALSE;
  TSepiConstant.Create(Result, 'S_FALSE', IntegerType).Value.WriteBuffer(I, 4);
  I := E_NOINTERFACE;
  TSepiConstant.Create(Result, 'E_NOINTERFACE',
    HRESULTType).Value.WriteBuffer(I, 4);
  I := E_UNEXPECTED;
  TSepiConstant.Create(Result, 'E_UNEXPECTED',
    HRESULTType).Value.WriteBuffer(I, 4);
  I := E_NOTIMPL;
  TSepiConstant.Create(Result, 'E_NOTIMPL',
    HRESULTType).Value.WriteBuffer(I, 4);

  { ... }
end;

initialization
  SepiRegisterImportedUnit('System', ImportUnit);
end.

