{*
  Définit les classes de gestion des types composites
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiCompTypes;

interface

uses
  Classes, SysUtils, ScUtils, SepiMetaUnits, SepiMetaMembers, SysConst, TypInfo,
  ScLists;

type
  {*
    Type enregistrement
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiRecordType = class(TSepiType)
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string);

    procedure AddField(const FieldName : string;
      FieldType : TSepiType); overload;
    procedure AddField(const FieldName : string;
      FieldTypeInfo : PTypeInfo); overload;

    function CompatibleWith(AType : TSepiType) : boolean; override;
  end;

  TSepiObjectType = class end;

  {*
    Type classe
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiClassType = class(TSepiType)
  private
    FAncestor : TSepiClassType;    /// Ancêtre
    FDelphiClass : TClass;         /// Classe Delphi d'importation
    FObjectType : TSepiObjectType; /// Type objet correspondant
  protected
    procedure ChildAdded(Child : TSepiMeta); override;

    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AAncestor : TSepiClassType; ADelphiClass : TClass;
      AObjectType : TSepiObjectType);

    function CompatibleWith(AType : TSepiType) : boolean; override;
    function InheritsFrom(AParent : TSepiClassType) : boolean;

    property Ancestor : TSepiClassType read FAncestor;
    property DelphiClass : TClass read FDelphiClass;
    property ObjectType : TSepiObjectType read FObjectType;
  end;

  (*{*
    Type objet
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiObjectType = class(TSepiMetaMemberContainer)
  private
    FAncestor : TSepiObjectType;                /// Ancêtre
    FDelphiClass : TClass;                      /// Classe Delphi d'importation
    FClassType : TSepiClassType;                /// Type classe correspondant
    FCurrentVisibility : TSepiMemberVisibility; /// Visibilité courante
  protected
    procedure ChildAdded(Child : TSepiMeta); override;

    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AAncestor : TSepiObjectType; ADelphiClass : TClass);

    function CompatibleWith(AType : TSepiType) : boolean; override;
    function InheritsFrom(AParent : TSepiObjectType) : boolean;

    property Ancestor : TSepiObjectType read FAncestor;
    property ObjClassType : TSepiClassType read FClassType;
    property CurrentVisibility : TSepiMemberVisibility
      read FCurrentVisibility write FCurrentVisibility;
  end;*)

  {*
    Type référence de méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMethodRefType = class(TSepiType)
  private
    FSignature : TSepiMethodSignature; /// Signature
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName, ASignature : string;
      ACallConvention : TCallConvention = ccRegister);
    destructor Destroy; override;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property Signature : TSepiMethodSignature read FSignature;
  end;

implementation

const
  // Tailles de structure TTypeData en fonction des types
  RecordTypeDataLength = 0;
  ClassTypeDataLengthBase = sizeof(TClass) + sizeof(Pointer) + sizeof(SmallInt);
  IntfTypeDataLengthBase =
    sizeof(Pointer) + sizeof(TIntfFlagsBase) + sizeof(TGUID);

{------------------------}
{ Classe TSepiRecordType }
{------------------------}

{*
  Charge un type record depuis un flux
*}
constructor TSepiRecordType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  LoadChildren(Stream);
end;

{*
  Crée un nouveau type record
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
*}
constructor TSepiRecordType.Create(AOwner : TSepiMeta; const AName : string);
begin
  inherited Create(AOwner, AName, tkRecord);
end;

{*
  Ajoute un champ au record
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
*}
procedure TSepiRecordType.AddField(const FieldName : string;
  FieldType : TSepiType);
begin
  TSepiMetaField.Create(Self, FieldName, FieldType);
end;

{*
  Ajoute un champ au record
  @param FieldName   Nom du champ
  @param FieldType   RTTI du type du champ
*}
procedure TSepiRecordType.AddField(const FieldName : string;
  FieldTypeInfo : PTypeInfo);
begin
  AddField(FieldName, Root.FindTypeByTypeInfo(FieldTypeInfo));
end;

{*
  [@inheritDoc]
*}
function TSepiRecordType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := Self = AType;
end;

{-----------------------}
{ Classe TSepiClassType }
{-----------------------}

constructor TSepiClassType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FAncestor, 4);
  FDelphiClass := nil;
  Stream.ReadBuffer(FObjectType, 4);
end;

constructor TSepiClassType.Create(AOwner : TSepiMeta; const AName : string;
  AAncestor : TSepiClassType; ADelphiClass : TClass;
  AObjectType : TSepiObjectType);
begin
  inherited Create(AOwner, AName, tkClass);
  FAncestor := AAncestor;
  FDelphiClass := ADelphiClass;
  FObjectType := AObjectType;
end;

procedure TSepiClassType.ChildAdded(Child : TSepiMeta);
begin
  inherited;
  //Child.Visibility := FObjectType.FCurrentVisibility;
end;

procedure TSepiClassType.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FAncestor);
  FDelphiClass := FAncestor.DelphiClass;
  OwningUnit.LoadRef(FObjectType);
end;

function TSepiClassType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType.Kind = tkClass) and
    TSepiClassType(AType).InheritsFrom(Self);
end;

function TSepiClassType.InheritsFrom(AParent : TSepiClassType) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FAncestor) and FAncestor.InheritsFrom(AParent));
end;

(*{------------------------}
{ Classe TSepiObjectType }
{------------------------}

constructor TSepiObjectType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FAncestor, 4);
  FDelphiClass := nil;
  Stream.ReadBuffer(FClassType, 4);
  FCurrentVisibility := mvPrivate;
end;

constructor TSepiObjectType.Create(AOwner : TSepiMeta; const AName : string;
  AAncestor : TSepiObjectType; ADelphiClass : TClass);
begin
  inherited Create(AOwner, AName, tkUnknown);
  FAncestor := AAncestor;
  FDelphiClass := ADelphiClass;
  FClassType := TSepiClassType.Create(AOwner, 'CLASS$'+AName,
    AAncestor.ObjClassType, ADelphiClass, Self);
  FCurrentVisibility := mvPrivate;
end;

procedure TSepiObjectType.ChildAdded(Child : TSepiMeta);
begin
  inherited;
  Child.Visibility := FCurrentVisibility;
end;

procedure TSepiObjectType.Loaded;
begin
  OwningUnit.LoadRef(FAncestor);
  OwningUnit.LoadRef(FClassType);
  FDelphiClass := FClassType.DelphiClass;
end;

function TSepiObjectType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType.Kind = tkObject) and
    TSepiObjectType(AType).InheritsFrom(Self);
end;

function TSepiObjectType.InheritsFrom(AParent : TSepiObjectType) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FAncestor) and FAncestor.InheritsFrom(AParent));
end;*)

{---------------------------}
{ Classe TSepiMethodRefType }
{---------------------------}

{*
  Recense un type référence de méthode natif
*}
constructor TSepiMethodRefType.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;

  { TODO 2 : Recenser un type référence de méthode natif }
end;

{*
  Charge un type référence de méthode depuis un flux
*}
constructor TSepiMethodRefType.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  FSignature := TSepiMethodSignature.Load(Self, Stream);
end;

{*
  Crée un nouveau type référence de méthode
  @param AOwner            Propriétaire du type
  @param AName             Nom du type
  @param ASignature        Signature
  @param ACallConvention   Convention d'appel
*}
constructor TSepiMethodRefType.Create(AOwner : TSepiMeta;
  const AName, ASignature : string;
  ACallConvention : TCallConvention = ccRegister);
begin
  inherited Create(AOwner, AName, tkMethod);
  FSignature := TSepiMethodSignature.Create(Self, ASignature, ACallConvention);
end;

{*
  Détruit l'instance
*}
destructor TSepiMethodRefType.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

{*
  [@inheritDoc]
*}
function TSepiMethodRefType.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType.Kind = tkMethod) and
    FSignature.Equals(TSepiMethodRefType(AType).FSignature);
end;

initialization
  SepiRegisterMetaClasses([
    TSepiRecordType, TSepiClassType, TSepiMethodRefType
  ]);
end.

