{*
  Définit les classes de gestion des types chaîne
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiStrTypes;

interface

uses
  Classes, SysUtils, ScUtils, SepiMetaUnits, SysConst, TypInfo, ScLists;

type
  {*
    Type chaîne de caractères courte
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiShortStringType = class(TSepiType)
  private
    FMaxLength : integer; /// Longueur maximale
  protected
    procedure ExtractTypeData; override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AMaxLength : integer = 255);

    property MaxLength : integer read FMaxLength;
  end;

  {*
    Type chaîne de caractères longue
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiStringType = class(TSepiType)
  private
    FIsUnicode : boolean; /// Indique si la chaîne est Unicode ou non
  protected
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
  Recense un type chaîne courte natif
*}
constructor TSepiShortStringType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type chaîne courte depuis un flux
*}
constructor TSepiShortStringType.Load(AOwner : TSepiMeta; Stream : TStream);
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
constructor TSepiShortStringType.Create(AOwner : TSepiMeta;
  const AName : string; AMaxLength : integer = 255);
begin
  inherited Create(AOwner, AName, tkString);

  AllocateTypeInfo(ShortStringTypeDataLength);
  TypeData.MaxLength := AMaxLength;

  ExtractTypeData;
end;

{*
  [@inheritedDoc]
*}
procedure TSepiShortStringType.ExtractTypeData;
begin
  inherited;

  FMaxLength := TypeData.MaxLength;
  FSize := FMaxLength + 1;
end;

{------------------------}
{ Classe TSepiStringType }
{------------------------}

{*
  Recense un type chaîne longue natif
*}
constructor TSepiStringType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;
  ExtractTypeData;
end;

{*
  Charge un type chaîne longue depuis un flux
*}
constructor TSepiStringType.Load(AOwner : TSepiMeta; Stream : TStream);
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
  [@inheritedDoc]
*}
procedure TSepiStringType.ExtractTypeData;
begin
  inherited;

  FSize := 4;
  FIsUnicode := Kind = tkWString;
end;

initialization
  SepiRegisterMetaClasses([
    TSepiShortStringType, TSepiStringType
  ]);
end.

