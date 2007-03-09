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
  private
    FPacked : boolean;

    function NextOffset(Field : TSepiMetaField) : integer;
    function AddField(const FieldName : string; FieldType : TSepiType;
      After : TSepiMetaField) : TSepiMetaField; overload;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      APacked : boolean = False);

    function AddField(const FieldName : string;
      FieldType : TSepiType) : TSepiMetaField; overload;
    function AddField(const FieldName : string; FieldType : TSepiType;
      const After : string) : TSepiMetaField; overload;
    function AddField(const FieldName : string;
      FieldTypeInfo : PTypeInfo) : TSepiMetaField; overload;
    function AddField(const FieldName : string; FieldTypeInfo : PTypeInfo;
      const After : string) : TSepiMetaField; overload;

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property IsPacked : boolean read FPacked;
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
      AOfObject : boolean = False;
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

  Stream.ReadBuffer(FPacked, 1);
  LoadChildren(Stream);
end;

{*
  Crée un nouveau type record
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
*}
constructor TSepiRecordType.Create(AOwner : TSepiMeta; const AName : string;
  APacked : boolean = False);
begin
  inherited Create(AOwner, AName, tkRecord);
  FPacked := APacked;
end;

{*
  Détermine l'offset d'un champ suivant un champ donné en mémoire
  @param Field   Champ déjà existant, précédent le nouveau
  @return Offset du nouveau champ
*}
function TSepiRecordType.NextOffset(Field : TSepiMetaField) : integer;
begin
  Result := Field.Offset + Field.FieldType.Size;
  { TODO 2 : Aligner les champs dans un record non packed }
end;

{*
  Ajoute un champ au record
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param After       Champ précédent en mémoire
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldType : TSepiType; After : TSepiMetaField) : TSepiMetaField;
var Offset : integer;
begin
  if After = nil then Offset := 0 else
    Offset := NextOffset(After);

  Result := TSepiMetaField.Create(Self, FieldName, FieldType, Offset);

  if Offset + FieldType.Size > FSize then
    FSize := Offset + FieldType.Size;
end;

{*
  Ajoute un champ au record
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldType : TSepiType) : TSepiMetaField;
var LastField : TSepiMetaField;
begin
  if ChildCount = 0 then LastField := nil else
    LastField := TSepiMetaField(Children[ChildCount-1]);

  Result := AddField(FieldName, FieldType, LastField);
end;

{*
  Ajoute un champ au record après un champ donné en mémoire
  @param FieldName   Nom du champ
  @param FieldType   Type du champ
  @param After       Nom du champ précédent en mémoire (vide pour le début)
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldType : TSepiType; const After : string) : TSepiMetaField;
begin
  Result := AddField(FieldName, FieldType, TSepiMetaField(FindMeta(After)));
end;

{*
  Ajoute un champ au record
  @param FieldName       Nom du champ
  @param FieldTypeInto   RTTI du type du champ
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldTypeInfo : PTypeInfo) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeInfo));
end;

{*
  Ajoute un champ au record après un champ donné en mémoire
  @param FieldName       Nom du champ
  @param FieldTypeInfo   RTTI du type du champ
  @param After           Nom du champ précédent en mémoire (vide pour le début)
  @return Champ nouvellement ajouté
*}
function TSepiRecordType.AddField(const FieldName : string;
  FieldTypeInfo : PTypeInfo; const After : string) : TSepiMetaField;
begin
  Result := AddField(FieldName, Root.FindType(FieldTypeInfo),
    TSepiMetaField(FindMeta(After)));
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
  FSignature := TSepiMethodSignature.RegisterTypeData(Self, TypeData);
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
  @param AOfObject         Indique s'il s'agit d'une méthode
  @param ACallConvention   Convention d'appel
*}
constructor TSepiMethodRefType.Create(AOwner : TSepiMeta;
  const AName, ASignature : string; AOfObject : boolean = False;
  ACallConvention : TCallConvention = ccRegister);
var Prefix : string;
begin
  inherited Create(AOwner, AName, tkMethod);

  if AOfObject then Prefix := '' else Prefix := 'unit ';
  FSignature := TSepiMethodSignature.Create(Self,
    Prefix + ASignature, ACallConvention);
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

