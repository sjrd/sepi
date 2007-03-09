{*
  Définit les classes de meta-membres
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiMetaMembers;

interface

uses
  SysUtils, Classes, StrUtils, TypInfo, ScStrUtils, SepiMetaUnits;

type
  {*
    Type de liaison d'une méthode
  *}
  TMethodLinkKind = (mlkStatic, mlkVirtual, mlkDynamic);

  {*
    Convention d'appel d'une méthode
  *}
  TCallConvention = (ccRegister, ccStdCall, ccPascal, ccCDecl);

  {*
    Type d'accesseur d'une propriété
  *}
  TPropertyAccessKind = (pakNone, pakField, pakMethod);

  {*
    Meta-variable
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaField = class(TSepiMeta)
  private
    FType : TSepiType; /// Type du champ
    FOffset : integer; /// Offset
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AType : TSepiType; AOffset : integer);

    property FieldType : TSepiType read FType;
    property Offset : integer read FOffset;
  end;

  {*
    Meta-paramètre de méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaParam = class(TSepiMeta)
  private
    FType : TSepiType;    /// Type du paramètre
    FFlags : TParamFlags; /// Flags du paramètre

    constructor RegisterParamData(AOwner : TSepiMeta; var ParamData : Pointer);
    constructor CreateFromString(AOwner : TSepiMeta; const Definition : string);
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      AType : TSepiType; AFlags : TParamFlags = []);

    function Equals(AParam : TSepiMetaParam) : boolean;
    function CompatibleWith(AType : TSepiType) : boolean;

    property ParamType : TSepiType read FType;
    property Flags : TParamFlags read FFlags;
  end;

  {*
    Signature d'une méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMethodSignature = class
  private
    FOwner : TSepiMeta;                /// Propriétaire de la signature
    FKind : TMethodKind;               /// Type de méthode
    FReturnType : TSepiType;           /// Type de retour
    FCallConvention : TCallConvention; /// Convention d'appel

    procedure Loaded;

    function GetParamCount : integer;
    function GetParams(Index : integer) : TSepiMetaParam;
  public
    constructor RegisterTypeData(AOwner : TSepiMeta; ATypeData : PTypeData);
    constructor Load(AOwner : TSepiMeta; Stream : TStream);
    constructor Create(AOwner : TSepiMeta; const ASignature : string;
      ACallConvention : TCallConvention = ccRegister);

    function Equals(ASignature : TSepiMethodSignature) : boolean;
    function CompatibleWith(const ATypes : array of TSepiType) : boolean;

    property Kind : TMethodKind read FKind;
    property ParamCount : integer read GetParamCount;
    property Params[index : integer] : TSepiMetaParam read GetParams;
    property ReturnType : TSepiType read FReturnType;
    property CallConvention : TCallConvention read FCallConvention;
  end;

  {*
    Meta-méthode
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaMethod = class(TSepiMeta)
  private
    { TODO 2 -cMetaunités : Ajouter des champs concernant les directives de
      méthodes }
    FCode : Pointer;                   /// Adresse de code natif
    FSignature : TSepiMethodSignature; /// Signature de la méthode
    FLinkKind : TMethodLinkKind;       /// Type de liaison d'appel
    FAbstract : boolean;               /// Indique si la méthode est abstraite
    FInherited : TSepiMetaMethod;      /// Méthode héritée

    procedure FindInherited;
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string;
      ACode : Pointer; const ASignature : string;
      ACallConvention : TCallConvention = ccRegister;
      ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False);
    destructor Destroy; override;

    property Signature : TSepiMethodSignature read FSignature;
    property LinkKind : TMethodLinkKind read FLinkKind;
    property IsAbstract : boolean read FAbstract;
  end;

  {*
    Meta-méthode surchargée
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaOverloadedMethod = class(TSepiMeta)
  private
    FMethodCount : integer; /// Nombre de méthodes de même nom
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName : string);

    function NextID : integer;
    function FindMethod(const ATypes : array of TSepiType) : TSepiMetaMethod;
  end;

  {*
    Accès de propriété
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiPropertyAccess = record
    Kind : TPropertyAccessKind;               /// Type d'accès
    case TPropertyAccessKind of
      pakNone : (Meta : TSepiMeta);           /// Meta d'accès (neutre)
      pakField : (Field : TSepiMetaField);    /// Meta-champ d'accès
      pakMethod : (Method : TSepiMetaMethod); /// Meta-méthode d'accès
  end;

  {*
    Meta-propriété
    @author Sébastien Jean Robert Doeraene
    @version 1.0
  *}
  TSepiMetaProperty = class(TSepiMeta)
  private
    FSignature : TSepiMethodSignature;  /// Signature

    FReadAccess : TSepiPropertyAccess;  /// Accès en lecture
    FWriteAccess : TSepiPropertyAccess; /// Accès en écriture

    function GetPropType : TSepiType;
  protected
    procedure Loaded; override;
  public
    constructor Load(AOwner : TSepiMeta; Stream : TStream); override;
    constructor Create(AOwner : TSepiMeta; const AName, ASignature : string;
      AReadAccess, AWriteAccess : TSepiMeta);
    destructor Destroy; override;

    property Signature : TSepiMethodSignature read FSignature;
    property PropType : TSepiType read GetPropType;

    property ReadAccess : TSepiPropertyAccess read FReadAccess;
    property WriteAccess : TSepiPropertyAccess read FWriteAccess;
  end;

const
  /// Procédure d'unité
  mkUnitProcedure = TMethodKind(integer(High(TMethodKind))+1);
  /// Fonction d'unité
  mkUnitFunction = TMethodKind(integer(High(TMethodKind))+2);
  /// Propriété
  mkProperty = TMethodKind(integer(High(TMethodKind))+3);

  /// Chaînes des types de méthode
  MethodKindStrings : array[mkProcedure..mkProperty] of string = (
    'procedure', 'function', 'constructor', 'destructor',
    'class procedure', 'class function', 'class constructor',
    '', '', '', 'unit procedure', 'unit function', 'property'
  );

implementation

{-----------------------}
{ Classe TSepiMetaField }
{-----------------------}

{*
  Charge un champ depuis un flux
*}
constructor TSepiMetaField.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FType, 4);
end;

{*
  Crée un nouveau champ
  @param AOwner   Propriétaire du champ
  @param AName    Nom du champ
  @param AType    Type du champ
*}
constructor TSepiMetaField.Create(AOwner : TSepiMeta; const AName : string;
  AType : TSepiType; AOffset : integer);
begin
  inherited Create(AOwner, AName);
  FType := AType;
  FOffset := AOffset;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaField.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FType);
end;

{-----------------------}
{ Classe TSepiMetaParam }
{-----------------------}

{*
  Crée un paramètre depuis les données de type d'une référence de méthode
  En sortie, le pointeur ParamData a avancé jusqu'au paramètre suivant
  @param AOwner      Propriétaire du paramètre
  @param ParamData   Pointeur vers les données du paramètre
*}
constructor TSepiMetaParam.RegisterParamData(AOwner : TSepiMeta;
  var ParamData : Pointer);
var AFlags : TParamFlags;
    AName, ATypeStr : string;
begin
  AFlags := TParamFlags(ParamData^);
  inc(LongInt(ParamData), sizeof(TParamFlags));
  AName := PShortString(ParamData)^;
  inc(LongInt(ParamData), PByte(ParamData)^ + 1);
  ATypeStr := PShortString(ParamData)^;
  inc(LongInt(ParamData), PByte(ParamData)^ + 1);

  Create(AOwner, AName, AOwner.Root.FindType(ATypeStr), AFlags);
end;

{*
  Crée un paramètre depuis sa définition Delphi
  @param AOwner       Propriétaire du paramètre
  @param Definition   Définition Delphi du paramètre
*}
constructor TSepiMetaParam.CreateFromString(AOwner : TSepiMeta;
  const Definition : string);
var AFlags : TParamFlags;
    NamePart, TypePart, FlagStr, AName, ATypeStr : string;
begin
  AFlags := [];
  SplitToken(Definition, ':', NamePart, TypePart);

  // Partie du nom - à gauche du :
  if SplitToken(Trim(NamePart), ' ', FlagStr, AName) then
  begin
    case AnsiIndexText(FlagStr, ['var', 'const', 'out']) of {don't localize}
      0 : Include(AFlags, pfVar);
      1 : Include(AFlags, pfConst);
      2 : Include(AFlags, pfOut);
    end;
  end;

  // Partie du type - à droite du :
  TypePart := Trim(TypePart);
  if AnsiStartsText('array of ', TypePart) then {don't localize}
  begin
    Include(AFlags, pfArray);
    ATypeStr := TrimLeft(Copy(TypePart, 10, MaxInt)); // 10 is 'array of |'
  end else ATypeStr := TypePart;

  Create(AOwner, AName, AOwner.Root.FindType(ATypeStr), AFlags);
end;

{*
  Charge un paramètre depuis un flux
*}
constructor TSepiMetaParam.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  Stream.ReadBuffer(FType, 4);
  Stream.ReadBuffer(FFlags, 1);
end;

{*
  Crée un nouveau paramètre
  @param AOwner   Propriétaire du paramètre
  @param AName    Nom du paramètre
  @param AType    Type du paramètre
  @param AFlags   Flags
*}
constructor TSepiMetaParam.Create(AOwner : TSepiMeta; const AName : string;
  AType : TSepiType; AFlags : TParamFlags = []);
begin
  inherited Create(AOwner, AName);

  FType := AType;
  FFlags := AFlags;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaParam.Loaded;
begin
  inherited;
  OwningUnit.LoadRef(FType);
end;

{*
  Détermine si deux paramètres sont identiques
  @param AParam   Paramètre à comparer
  @return True si les paramètres sont identiques, False sinon
*}
function TSepiMetaParam.Equals(AParam : TSepiMetaParam) : boolean;
begin
  Result := (ParamType = AParam.ParamType) and (Flags = AParam.Flags);
end;

{*
  Détermine si un type est compatible avec le paramètre
  @param AType   Type à Tester
  @return True si le type est compatible, False sinon
*}
function TSepiMetaParam.CompatibleWith(AType : TSepiType) : boolean;
begin
  Result := FType.CompatibleWith(AType);
end;

{-----------------------------}
{ Classe TSepiMethodSignature }
{-----------------------------}

{*
  Crée une signature à partir des données de type d'un type méthode
  @param AOwner      Propriétaire de la signature
  @param ATypeData   Données de type
*}
constructor TSepiMethodSignature.RegisterTypeData(AOwner : TSepiMeta;
  ATypeData : PTypeData);
var ParamData : Pointer;
    I : integer;
begin
  inherited Create;

  FOwner := AOwner;
  FKind := ATypeData.MethodKind;
  ParamData := @ATypeData.ParamList;

  for I := 1 to ATypeData.ParamCount do
    TSepiMetaParam.RegisterParamData(FOwner, ParamData);

  FReturnType := FOwner.Root.FindType(PShortString(ParamData)^);
  FCallConvention := ccRegister;
end;

{*
  Charge une signature depuis un flux
  @param AOwner   Propriétaire de la signature
  @param Stream   Flux depuis lequel charger la signature
*}
constructor TSepiMethodSignature.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited Create;

  FOwner := AOwner;
  Stream.ReadBuffer(FKind, 1);
  Stream.ReadBuffer(FReturnType, 4);
  Stream.ReadBuffer(FCallConvention, 1);

  // Parameters should be loaded by the owner, for they are children of it
end;

{*
  Crée une signature de méthode
  @param AOwner            Propriétaire de la signature
  @param ASignature        Signature Delphi
  @param ACallConvention   Convention d'appel
*}
constructor TSepiMethodSignature.Create(AOwner : TSepiMeta;
  const ASignature : string; ACallConvention : TCallConvention = ccRegister);

  function MultiPos(const Chars : array of Char; const Str : string;
    Offset : integer = 1) : integer;
  var I : integer;
  begin
    for I := Low(Chars) to High(Chars) do
    begin
      Result := PosEx(Chars[I], Str, Offset);
      if Result > 0 then exit;
    end;
    Result := Length(Str)+1;
  end;

var ParamPos, ReturnTypePos, ParamEnd : integer;
begin
  inherited Create;

  FOwner := AOwner;
  FKind := mkProcedure;
  FReturnType := nil;
  FCallConvention := ACallConvention;

  // Type de méthode
  ParamPos := MultiPos(['(', '[', ':'], ASignature);
  FKind := TMethodKind(AnsiIndexText(
    Trim(Copy(ASignature, 1, ParamPos-1)), MethodKindStrings));

  // Type de retour
  if (Kind = mkFunction) or (Kind = mkUnitFunction) or (Kind = mkProperty) then
  begin
    ReturnTypePos := RightPos(':', ASignature);
    FReturnType := FOwner.Root.FindType(
      Trim(Copy(ASignature, ReturnTypePos+1, MaxInt)));
  end;

  // Paramètres
  if ParamPos <= Length(ASignature) then
  begin
    while not (ASignature[ParamPos] in [')', ']', ':']) do
    begin
      inc(ParamPos);
      ParamEnd := MultiPos([';', ')', ']', ':'], ASignature, ParamPos);
      TSepiMetaParam.CreateFromString(FOwner,
        Copy(ASignature, ParamPos, ParamEnd-ParamPos));
      ParamPos := ParamEnd;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMethodSignature.Loaded;
begin
  inherited;
  FOwner.OwningUnit.LoadRef(FReturnType);
end;

{*
  Nombre de paramètres
  @return Nombre de paramètres
*}
function TSepiMethodSignature.GetParamCount : integer;
begin
  Result := FOwner.ChildCount;
end;

{*
  Tableau zero-based des paramètres
  @param Index   Index du paramètre à récupérer
  @return Paramètre situé à l'index Index
*}
function TSepiMethodSignature.GetParams(Index : integer) : TSepiMetaParam;
begin
  { Here, we can't guarantee that all children are TSepiMetaParam, so we
    check it with an as operator. }
  Result := FOwner.Children[Index] as TSepiMetaParam;
end;

{*
  Détermine si deux signatures sont identiques
  @param ASignature   Signature à comparer
  @return True si les signatures sont identiques, False sinon
*}
function TSepiMethodSignature.Equals(
  ASignature : TSepiMethodSignature) : boolean;
var I : integer;
begin
  Result := False;

  if Kind <> ASignature.Kind then exit;
  if ParamCount <> ASignature.ParamCount then exit;
  for I := 0 to ParamCount-1 do
    if not Params[I].Equals(ASignature.Params[I]) then exit;
  if FReturnType <> ASignature.ReturnType then exit;

  Result := True;
end;

{*
  Détermine si une liste de types est compatible avec la signature
  @param ATypes   Liste des types à tester
  @return True si la liste de types est compatible, False sinon
*}
function TSepiMethodSignature.CompatibleWith(
  const ATypes : array of TSepiType) : boolean;
var I : integer;
begin
  Result := False;

  if ParamCount <> Length(ATypes) then exit;
  for I := 0 to ParamCount-1 do
    if not Params[I].CompatibleWith(ATypes[Low(ATypes)+I]) then exit;

  Result := True;
end;

{------------------------}
{ Classe TSepiMetaMethod }
{------------------------}

{*
  Charge une meta-méthode depuis un flux
*}
constructor TSepiMetaMethod.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  FCode := nil;
  FSignature := TSepiMethodSignature.Load(Self, Stream);
  LoadChildren(Stream);
  Stream.ReadBuffer(FLinkKind, 1);
  Stream.ReadBuffer(FAbstract, 1);
  FInherited := nil;
end;

{*
  Crée une nouvelle meta-méthode
  @param AOwner       Propriétaire de la méthode
  @param AName        Nom de la méthode
  @param ASignature   Signature Delphi de la méthode
  @param ALinkKind    Type de liaison
  @param AAbstract    Indique si la méthode est abstraite
*}
constructor TSepiMetaMethod.Create(AOwner : TSepiMeta; const AName : string;
  ACode : Pointer; const ASignature : string;
  ACallConvention : TCallConvention = ccRegister;
  ALinkKind : TMethodLinkKind = mlkStatic; AAbstract : boolean = False);
begin
  inherited Create(AOwner, AName);

  FCode := ACode;
  FSignature := TSepiMethodSignature.Create(Self, ASignature, ACallConvention);
  FLinkKind := ALinkKind;
  FAbstract := AAbstract and (FLinkKind <> mlkStatic);
  FindInherited;
end;

{*
  Détruit l'instance
*}
destructor TSepiMetaMethod.Destroy;
begin
  FSignature.Free;
  inherited Destroy;
end;

{*
  Trouve la méthode héritée de même nom
*}
procedure TSepiMetaMethod.FindInherited;
var Overloaded : boolean;
    LookFor : string;
begin
  LookFor := Name;

  Overloaded := Copy(Name, 1, 3) = 'OL$';
  if Overloaded then
  begin
    Delete(LookFor, 1, 3);
    Delete(LookFor, Pos('$', LookFor), Length(LookFor));
  end;

  FInherited := nil;
  { TODO 3 -cMetaunités : Trouver la méthode héritée }
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaMethod.Loaded;
begin
  inherited;
  FSignature.Loaded;
  FindInherited;
end;

{----------------------------------}
{ Classe TSepiMetaOverloadedMethod }
{----------------------------------}

{*
  Charge une méthode surchargée depuis un flux
*}
constructor TSepiMetaOverloadedMethod.Load(AOwner : TSepiMeta;
  Stream : TStream);
begin
  inherited;
  Stream.ReadBuffer(FMethodCount, 4);
end;

{*
  Crée une nouvelle méthode surchargée
  @param AOwner   Propriétaire de la méthode
  @param AName    Nom de la méthode
*}
constructor TSepiMetaOverloadedMethod.Create(AOwner : TSepiMeta;
  const AName : string);
begin
  inherited Create(AOwner, AName);
  FMethodCount := 0;
end;

{*
  Détermine l'ID de méthode surchargée suivant
  @return Nouvel ID
*}
function TSepiMetaOverloadedMethod.NextID : integer;
begin
  Result := FMethodCount;
  inc(FMethodCount);
end;

{*
  Trouve la méthode effective qui correspond à une liste de types de paramètres
  @param ATypes   Liste des types de paramètres
  @return Méthode effective correspondante
*}
function TSepiMetaOverloadedMethod.FindMethod(
  const ATypes : array of TSepiType) : TSepiMetaMethod;
var I : integer;
begin
  for I := 0 to FMethodCount-1 do
  begin
    Result := TSepiMetaMethod(Owner.FindMeta(Format('OL$%s$%d', [Name, I])));
    if Result.Signature.CompatibleWith(ATypes) then exit;
  end;
  Result := nil;
end;

{--------------------------}
{ Classe TSepiMetaProperty }
{--------------------------}

{*
  Charge une propriété depuis un flux
*}
constructor TSepiMetaProperty.Load(AOwner : TSepiMeta; Stream : TStream);
begin
  inherited;

  FSignature := TSepiMethodSignature.Load(AOwner, Stream);
  LoadChildren(Stream);

  Stream.ReadBuffer(FReadAccess, sizeof(TSepiPropertyAccess));
  Stream.ReadBuffer(FWriteAccess, sizeof(TSepiPropertyAccess));
end;

{*
  Crée une nouvelle propriété
  @param AOwner         Propriétaire de la propriété
  @param AName          Nom de la propriété
  @param ASignature     Signature
  @param AReadAccess    Accès en lecture à la propriété (peut être nul)
  @param AWriteAccess   Accès en écriture à la propriété (peut être nul)
*}
constructor TSepiMetaProperty.Create(AOwner : TSepiMeta;
  const AName, ASignature : string; AReadAccess, AWriteAccess : TSepiMeta);
begin
  inherited Create(AOwner, AName);

  FSignature := TSepiMethodSignature.Create(Self, ASignature);

  FReadAccess.Meta := AReadAccess;
  if AReadAccess is TSepiMetaField then
    FReadAccess.Kind := pakField
  else if AReadAccess is TSepiMetaMethod then
    FReadAccess.Kind := pakMethod
  else
    FReadAccess.Kind := pakNone;

  FWriteAccess.Meta := AWriteAccess;
  if AWriteAccess is TSepiMetaField then
    FWriteAccess.Kind := pakField
  else if AWriteAccess is TSepiMetaMethod then
    FWriteAccess.Kind := pakMethod
  else
    FWriteAccess.Kind := pakNone;
end;

{*
  Détruit l'instance
*}
destructor TSepiMetaProperty.Destroy;
begin
  FSignature.Free;
  inherited;
end;

{*
  Type de la propriété
  @return Type de la propriété
*}
function TSepiMetaProperty.GetPropType : TSepiType;
begin
  Result := Signature.ReturnType;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMetaProperty.Loaded;
begin
  inherited;

  OwningUnit.LoadRef(FReadAccess.Meta);
  OwningUnit.LoadRef(FWriteAccess.Meta);
end;

initialization
  SepiRegisterMetaClasses([
    TSepiMetaField, TSepiMetaParam, TSepiMetaMethod, TSepiMetaOverloadedMethod,
    TSepiMetaProperty
  ]);
end.

