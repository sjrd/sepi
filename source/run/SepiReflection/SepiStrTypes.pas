{*
  Définit les classes de gestion des types chaîne
  @author sjrd
  @version 1.0
*}
unit SepiStrTypes;

interface

uses
  Classes, TypInfo, SepiReflectionCore;

type
  {*
    Type chaîne de caractères courte
    @author sjrd
    @version 1.0
  *}
  TSepiShortStringType = class(TSepiType)
  private
    FMaxLength: Integer; /// Longueur maximale
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;

    function GetAlignment: Integer; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AMaxLength: Integer = 255);

    property MaxLength: Integer read FMaxLength;
  end;

  {*
    Type chaîne de caractères longue
    @author sjrd
    @version 1.0
  *}
  TSepiStringType = class(TSepiType)
  private
    FIsUnicode: Boolean; /// Indique si la chaîne est Unicode ou non
  protected
    procedure Save(Stream: TStream); override;

    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner: TSepiMeta;
      ATypeInfo: PTypeInfo); override;
    constructor Load(AOwner: TSepiMeta; Stream: TStream); override;
    constructor Create(AOwner: TSepiMeta; const AName: string;
      AIsUnicode: Boolean = False);

    property IsUnicode: Boolean read FIsUnicode;
  end;

implementation

const
  // Tailles de structure TTypeData en fonction des types
  ShortStringTypeDataLength = SizeOf(Byte);
  StringTypeDataLength = 0;

{-----------------------------}
{ Classe TSepiShortStringType }
{-----------------------------}

{*
  Recense un type chaîne courte natif
*}
constructor TSepiShortStringType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type chaîne courte depuis un flux
*}
constructor TSepiShortStringType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  AllocateTypeInfo(ShortStringTypeDataLength);
  Stream.ReadBuffer(TypeData^, ShortStringTypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type chaîne courte
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AMaxLength   Longueur maximale
*}
constructor TSepiShortStringType.Create(AOwner: TSepiMeta;
  const AName: string; AMaxLength: Integer = 255);
begin
  inherited Create(AOwner, AName, tkString);

  AllocateTypeInfo(ShortStringTypeDataLength);
  TypeData.MaxLength := AMaxLength;

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
procedure TSepiShortStringType.Save(Stream: TStream);
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
function TSepiShortStringType.GetAlignment: Integer;
begin
  Result := 1;
end;

{------------------------}
{ Classe TSepiStringType }
{------------------------}

{*
  Recense un type chaîne longue natif
*}
constructor TSepiStringType.RegisterTypeInfo(AOwner: TSepiMeta;
  ATypeInfo: PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type chaîne longue depuis un flux
*}
constructor TSepiStringType.Load(AOwner: TSepiMeta; Stream: TStream);
begin
  inherited;

  AllocateTypeInfo(StringTypeDataLength);
  Stream.ReadBuffer(TypeData^, StringTypeDataLength);

  ExtractTypeData;
end;

{*
  Crée un nouveau type chaîne longue
  @param AOwner       Propriétaire du type
  @param AName        Nom du type
  @param AIsUnicode   Indique si la chaîne est Unicode ou non
*}
constructor TSepiStringType.Create(AOwner: TSepiMeta; const AName: string;
  AIsUnicode: Boolean = False);
var
  AKind: TTypeKind;
begin
  if AIsUnicode then
    AKind := tkWString
  else
    AKind := tkLString;
  inherited Create(AOwner, AName, AKind);

  AllocateTypeInfo(StringTypeDataLength);

  ExtractTypeData;
end;

{*
  [@inheritDoc]
*}
procedure TSepiStringType.Save(Stream: TStream);
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

