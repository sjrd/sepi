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
    FPacked : boolean; /// Indique si le record est packed

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

  {*
    Classe (type objet)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiClass = class(TSepiType)
  private
    FDelphiClass : TClass;   /// Classe Delphi
    FParent : TSepiClass;    /// Classe parent
    FParentInfo : PTypeInfo; /// RTTI de la classe parent
    FCompleted : boolean;    /// Indique si la classe est entièrement définie
  protected
    procedure Loaded; override;
  public
    constructor RegisterTypeInfo(AOwner : TSepiMeta;
      ATypeInfo : PTypeInfo); override;
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AParent : TSepiClass);

    procedure Complete;

    function CompatibleWith(AType : TSepiType) : boolean; override;
    function InheritsFrom(AParent : TSepiClass) : boolean;

    property DelphiClass : TClass read FDelphiClass;
    property Parent : TSepiClass read FParent;
    property Completed : boolean read FCompleted;
  end;

  {*
    Meta-classe (type classe)
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaClass = class(TSepiType)
  private
    FClass : TSepiClass; /// Classe correspondante
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AClass : TSepiClass);

    function CompatibleWith(AType : TSepiType) : boolean; override;

    property SepiClass : TSepiClass read FClass;
  end;

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

{-------------------}
{ Classe TSepiClass }
{-------------------}

{*
  Recense une classe native
*}
constructor TSepiClass.RegisterTypeInfo(AOwner : TSepiMeta;
  ATypeInfo : PTypeInfo);
begin
  inherited;

  FDelphiClass := TypeData.ClassType;
  FParentInfo := TypeData.ParentInfo^;
  if Assigned(FParentInfo) then
    FParent := TSepiClass(Root.FindType(FParentInfo))
  else
    FParent := nil;
  FCompleted := False;
end;

{*
  Charge une classe depuis un flux
*}
constructor TSepiClass.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  FDelphiClass := nil;
  Stream.ReadBuffer(FParent, 4);
  FParentInfo := nil;
  FCompleted := False;

  LoadChildren(Stream);
end;

{*
  Crée une nouvelle classe
  @param AOwner    Propriétaire du type
  @param AName     Nom du type
  @param AParent   Classe parent
*}
constructor TSepiClass.Create(AOwner : TSepiMeta; const AName : string;
  AParent : TSepiClass);
begin
  inherited Create(AOwner, AName, tkClass);

  FDelphiClass := nil;
  if Assigned(AParent) then FParent := AParent else
    FParent := TSepiClass(Root.FindType(System.TypeInfo(TObject)));
  FParentInfo := Parent.TypeInfo;
  FCompleted := False;
end;

{*
  [@inheritDoc]
*}
procedure TSepiClass.Loaded;
begin
  inherited;

  OwningUnit.LoadRef(FParent);
  FParentInfo := Parent.TypeInfo;

  Complete;
end;

{*
  Termine la classe et construit ses RTTI si ce n'est pas déjà fait
*}
procedure TSepiClass.Complete;
begin
  FCompleted := True;
  if Assigned(TypeInfo) then exit;
end;

{*
  [@inheritDoc]
*}
function TSepiClass.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType is TSepiClass) and
    TSepiClass(AType).InheritsFrom(Self);
end;

{*
  Détermine si la classe hérite d'une classe donnée
  @param AParent   Ancêtre à tester
  @return True si la classe hérite de AParent, False sinon
*}
function TSepiClass.InheritsFrom(AParent : TSepiClass) : boolean;
begin
  Result := (AParent = Self) or
    (Assigned(FParent) and FParent.InheritsFrom(AParent));
end;

{-----------------------}
{ Classe TSepiMetaClass }
{-----------------------}

{*
  Charge une classe depuis un flux
*}
constructor TSepiMetaClass.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FClass, 4);
end;

{*
  Crée une nouvelle classe
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
  @param AClass   Classe correspondante
*}
constructor TSepiMetaClass.Create(AOwner : TSepiMeta; const AName : string;
  AClass : TSepiClass);
begin
  inherited Create(AOwner, AName, tkClass);
  FClass := AClass;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaClass.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FClass);
end;

{*
  [@inheritDoc]
*}
function TSepiMetaClass.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := (AType is TSepiMetaClass) and
    TSepiMetaClass(AType).SepiClass.InheritsFrom(SepiClass);
end;

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
    TSepiRecordType, TSepiClass, TSepiMethodRefType
  ]);
end.

