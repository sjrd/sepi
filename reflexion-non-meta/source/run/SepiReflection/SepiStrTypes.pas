{*
  D�finit les classes de gestion des types cha�ne
  @author sjrd
  @version 1.0
*}
unit SepiStrTypes;

interface

uses
  Classes, TypInfo, SepiReflectionCore;

type
  {*
    Type cha�ne de caract�res courte
    @author sjrd
    @version 1.0
  *}
  TSepiShortStringType = class(TSepiType)
  private
    FMaxLength : integer; /// Longueur maximale
  protected
    procedure Save(Stream : TStream); override;

    procedure ExtractTypeData; override;

    function GetAlignment : integer; override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AMaxLength : integer = 255);

    property MaxLength : integer read FMaxLength;
  end;

  {*
    Type cha�ne de caract�res longue
    @author sjrd
    @version 1.0
  *}
  TSepiStringType = class(TSepiType)
  private
    FIsUnicode : boolean; /// Indique si la cha�ne est Unicode ou non
  protected
    procedure Save(Stream : TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AIsUnicode : boolean = False);

    property IsUnicode : boolean read FIsUnicode;
  end;

implementation

const
  // Tailles de structure TTypeData en fonction des types
  ShortStringTypeDataLength = sizeof(Byte);
  StringTypeDataLength = 0;

{-----------------------------}
{ Classe TSepiShortStringType }
{-----------------------------}

{*
  Recense un type cha�ne courte natif
*}
constructor TSepiShortStringType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type cha�ne courte depuis un flux
*}
constructor TSepiShortStringType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(ShortStringTypeDataLength);
  Stream.ReadBuffer(TypeData^, ShortStringTypeDataLength);

  ExtractTypeData;
end;

{*
  Cr�e un nouveau type cha�ne courte
  @param AOwner       Propri�taire du type
  @param AName        Nom du type
  @param AMaxLength   Longueur maximale
*}
constructor TSepiShortStringType.Create(AOwner : TSepiMeta;
  const AName : string; AMaxLength : integer = 255);
begin
  inherited Create(AOwner, AName, tkString);

  AllocateTypeInfo(ShortStringTypeDataLength);
  TypeData.MaxLength := AMaxLength;

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
procedure TSepiShortStringType.Save(Stream : TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, ShortStringTypeDataLength);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiShortStringType.ExtractTypeData;
begin
  inherited;

  FMaxLength := TypeData.MaxLength;
  FSize := FMaxLength + 1;
  FParamBehavior.AlwaysByAddress := True;
end;

{*
  [@inheritDoc]
*}
function TSepiShortStringType.GetAlignment : integer;
begin
  Result := 1;
end;

{------------------------}
{ Classe TSepiStringType }
{------------------------}

{*
  Recense un type cha�ne longue natif
*}
constructor TSepiStringType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type cha�ne longue depuis un flux
*}
constructor TSepiStringType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  AllocateTypeInfo(StringTypeDataLength);
  Stream.ReadBuffer(TypeData^, StringTypeDataLength);

  ExtractTypeData;
end;

{*
  Cr�e un nouveau type cha�ne longue
  @param AOwner       Propri�taire du type
  @param AName        Nom du type
  @param AIsUnicode   Indique si la cha�ne est Unicode ou non
*}
constructor TSepiStringType.Create(AOwner : TSepiMeta; const AName : string;
  AIsUnicode : boolean = False);
var AKind : TTypeKind;
begin
  if AIsUnicode then AKind := tkWString else AKind := tkLString;
  inherited Create(AOwner, AName, AKind);

  AllocateTypeInfo(StringTypeDataLength);

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
procedure TSepiStringType.Save(Stream : TStream);
begin
  inherited;
  Stream.WriteBuffer(TypeData^, StringTypeDataLength);
end;

{*
  [@inheritedDoc]
*}
procedure TSepiStringType.ExtractTypeData;
begin
  inherited;

  FSize := 4;
  FNeedInit := True;
  FIsUnicode := Kind = tkWString;
end;

initialization
  SepiRegisterMetaClasses([
    TSepiShortStringType, TSepiStringType
  ]);
end.

