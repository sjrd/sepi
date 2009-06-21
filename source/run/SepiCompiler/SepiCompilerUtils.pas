unit SepiCompilerUtils;

interface

uses
  SysUtils, Classes, TypInfo, SepiReflectionCore, SepiOrdTypes, SepiMembers,
  SepiSystemUnit,
  SepiExpressions, SepiParseTrees, SepiLexerUtils, SepiParserUtils,
  SepiCompilerErrors, SepiCompiler, SepiCompilerConsts;

type
  {*
    Classe de base pour les constructeurs de membres de type composites
    @author sjrd
    @version 1.0
  *}
  TSepiMemberBuilder = class(TObject)
  private
    FOwner: TSepiComponent; /// Propriétaire du membre à construire

    FOwnerType: TSepiType;         /// Type propriétaire (si applicable)
    FOwnerRecord: TSepiRecordType; /// Record propriétaire (si applicable)
    FOwnerClass: TSepiClass;       /// Classe propriétaire (si applicable)
    FOwnerIntf: TSepiInterface;    /// Interface propriétaire (si applicable)

    FName: string; /// Nom du membre à construire

    FCurrentNode: TSepiParseTreeNode; /// Noeud courant pour les erreurs
  protected
    procedure MakeError(const ErrorMsg: string;
      Kind: TSepiErrorKind = ekError);

    function LookForMember(const MemberName: string): TSepiComponent;
    function LookForMemberOrError(const MemberName: string): TSepiComponent;

    property OwnerType: TSepiType read FOwnerType;
    property OwnerRecord: TSepiRecordType read FOwnerRecord;
    property OwnerClass: TSepiClass read FOwnerClass;
    property OwnerIntf: TSepiInterface read FOwnerIntf;
  public
    constructor Create(AOwner: TSepiComponent);

    {*
      Construit le membre
    *}
    procedure Build; virtual; abstract;

    property Owner: TSepiComponent read FOwner;
    property Name: string read FName write FName;

    property CurrentNode: TSepiParseTreeNode
      read FCurrentNode write FCurrentNode;
  end;

  {*
    Constructeur de propriété
    @author sjrd
    @version 1.0
  *}
  TSepiPropertyBuilder = class(TSepiMemberBuilder)
  private
    FSignature: TSepiSignature;     /// Signature
    FReadAccess: TSepiComponent;         /// Accesseur en lecture
    FWriteAccess: TSepiComponent;        /// Accesseur en écriture
    FIndex: Integer;                /// Index
    FIndexType: TSepiOrdType;       /// Type de l'index
    FDefaultValue: Integer;         /// Valeur par défaut
    FStorage: TSepiPropertyStorage; /// Spécificateur de stockage
    FIsDefault: Boolean;            /// True si c'est la propriété par défaut

    function ValidateFieldAccess(Field: TSepiField): Boolean;
    function ValidateMethodAccess(Method: TSepiMethod;
      IsWriteAccess: Boolean): Boolean;
    function ValidateAccess(Access: TSepiComponent; IsWriteAccess: Boolean): Boolean;

    function IsTypeValidForDefault(PropType: TSepiType): Boolean;
    function CheckDefaultAllowed: Boolean;
  public
    constructor Create(AOwner: TSepiComponent);
    destructor Destroy; override;

    function Redefine(Node: TSepiParseTreeNode): Boolean;

    function SetReadAccess(const ReadAccessName: string;
      Node: TSepiParseTreeNode): Boolean;
    function SetWriteAccess(const WriteAccessName: string;
      Node: TSepiParseTreeNode): Boolean;

    function SetIndex(const Value: ISepiReadableValue;
      Node: TSepiParseTreeNode): Boolean;
    function SetDefaultValue(const Value: ISepiReadableValue;
      Node: TSepiParseTreeNode): Boolean;
    function SetNoDefault(Node: TSepiParseTreeNode): Boolean;

    function SetIsDefault(Node: TSepiParseTreeNode): Boolean;

    procedure Build; override;

    property Signature: TSepiSignature read FSignature;
    property ReadAccess: TSepiComponent read FReadAccess;
    property WriteAccess: TSepiComponent read FWriteAccess;
    property Index: Integer read FIndex;
    property IndexType: TSepiOrdType read FIndexType;
    property DefaultValue: Integer read FDefaultValue;
    property Storage: TSepiPropertyStorage read FStorage;
    property IsDefault: Boolean read FIsDefault;
  end;

  {*
    Type transient utilisé par un compilateur
    Toute tentative de charger ou sauvegarder une instance de
    TSepiCompilerTransientType déclenchera une exception EAssertionFailed.
    @author sjrd
    @version 1.0
  *}
  TSepiCompilerTransientType = class(TSepiType)
  protected
    procedure Save(Stream: TStream); override;
  public
    constructor Load(AOwner: TSepiComponent; Stream: TStream); override;
    constructor Create(AOwner: TSepiComponent; const AName: string;
      AKind: TTypeKind = tkUnknown);
  end;

  {*
    Type transient ensemble vide
    @author sjrd
    @version 1.0
  *}
  TSepiEmptySetType = class(TSepiCompilerTransientType)
  end;

function RequireReadableValue(const Expression: ISepiExpression;
  out Value: ISepiReadableValue): Boolean;
function RequireWritableValue(const Expression: ISepiExpression;
  out Value: ISepiWritableValue): Boolean;

function ConstValueAsInt64(const Value: ISepiReadableValue): Int64;

procedure TryAndConvertValues(SystemUnit: TSepiSystemUnit;
  var LowerValue, HigherValue: ISepiReadableValue);

function CompileSepiSource(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList; SourceFile: TStrings;
  const DestFileName: TFileName; RootNodeClass: TSepiParseTreeRootNodeClass;
  RootSymbolClass: TSepiSymbolClass; LexerClass: TSepiCustomLexerClass;
  ParserClass: TSepiCustomParserClass): TSepiUnit;

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  Requiert une valeur qui peut être lue
  En cas d'erreur, Value est affectée à une valeur erronée
  @param Expression   Expression
  @param Value        En sortie : l'expression sous forme de valeur lisible
  @return True en cas de succès, False sinon
*}
function RequireReadableValue(const Expression: ISepiExpression;
  out Value: ISepiReadableValue): Boolean;
begin
  Result := Supports(Expression, ISepiReadableValue, Value);

  if not Result then
  begin
    Expression.MakeError(SReadableValueRequired);
    Value := TSepiErroneousValue.Create(Expression.SepiRoot);
    Value.AttachToExpression(TSepiExpression.Create(Expression));
  end;
end;

{*
  Requiert une valeur qui peut être écrite
  En cas d'erreur, Value est affectée à nil
  @param Expression   Expression
  @param Value        En sortie : l'expression sous forme de valeur à écrire
  @return True en cas de succès, False sinon
*}
function RequireWritableValue(const Expression: ISepiExpression;
  out Value: ISepiWritableValue): Boolean;
begin
  Result := Supports(Expression, ISepiWritableValue, Value);

  if not Result then
    Expression.MakeError(SReadableValueRequired);
end;

{*
  Lit une valeur constante comme un Int64
  @param Value   Valeur à lire
  @return Valeur sous forme d'Int64
*}
function ConstValueAsInt64(const Value: ISepiReadableValue): Int64;
var
  IntegerType: TSepiIntegerType;
begin
  if Value.ValueType is TSepiInt64Type then
    Result := Int64(Value.ConstValuePtr^)
  else
  begin
    IntegerType := TSepiIntegerType(Value.ValueType);

    if IntegerType.Signed then
      Result := IntegerType.ValueAsInteger(Value.ConstValuePtr^)
    else
      Result := IntegerType.ValueAsCardinal(Value.ConstValuePtr^);
  end;
end;

{*
  Essaie de convertir les valeurs pour qu'elles aient le même type
  @param SystemUnit    Unité système
  @param LowerValue    Valeur basse
  @param HigherValue   Valeur haute
*}
procedure TryAndConvertValues(SystemUnit: TSepiSystemUnit;
  var LowerValue, HigherValue: ISepiReadableValue);
var
  LowerType, HigherType, CommonType: TSepiType;
  IntLowerValue, IntHigherValue: Int64;
begin
  LowerType := LowerValue.ValueType;
  HigherType := HigherValue.ValueType;

  if (LowerType is TSepiEnumType) and (HigherType is TSepiEnumType) then
  begin
    // Enumeration types

    CommonType := TSepiEnumType(LowerType).BaseType;

    if TSepiEnumType(HigherType).BaseType = CommonType then
    begin
      if LowerType <> CommonType then
        LowerValue := TSepiCastOperator.CastValue(
          CommonType, LowerValue) as ISepiReadableValue;

      if HigherType <> CommonType then
        HigherValue := TSepiCastOperator.CastValue(
          CommonType, HigherValue) as ISepiReadableValue;
    end;
  end else
  begin
    // Integer or char types

    if ((LowerType is TSepiIntegerType) or (LowerType is TSepiInt64Type)) and
      ((HigherType is TSepiIntegerType) or (HigherType is TSepiInt64Type)) then
    begin
      // Integer types

      IntLowerValue := ConstValueAsInt64(LowerValue);
      IntHigherValue := ConstValueAsInt64(HigherValue);

      if (Integer(IntLowerValue) = IntLowerValue) and
        (Integer(IntHigherValue) = IntHigherValue) then
        CommonType := SystemUnit.Integer
      else if (Cardinal(IntLowerValue) = IntLowerValue) and
        (Cardinal(IntHigherValue) = IntHigherValue) then
        CommonType := SystemUnit.Cardinal
      else
        CommonType := SystemUnit.Int64;
    end else if (LowerType is TSepiCharType) and
      (HigherType is TSepiCharType) then
    begin
      // Char types

      if LowerType.Size >= HigherType.Size then
        CommonType := LowerType
      else
        CommonType := HigherType;
    end else
    begin
      // Error
      Exit;
    end;

    if LowerType <> CommonType then
      LowerValue := TSepiConvertOperation.ConvertValue(
        CommonType, LowerValue);

    if HigherType <> CommonType then
      HigherValue := TSepiConvertOperation.ConvertValue(
        CommonType, HigherValue) as ISepiReadableValue;
  end;
end;

{*
  Compile un fichier source Sepi
  @param SepiRoot          Racine Sepi
  @param Errors            Gestionnaire d'erreurs
  @param SourceFile        Source à compiler
  @param DestFileName      Nom du fichier de sortie
  @param RootNodeClass     Classe du noeud racine
  @param RootSymbolClass   Classe de symboles du noeud racine
  @param LexerClass        Classe de l'analyseur lexical
  @param ParserClass       Classe de l'analyseur syntaxique
  @return Unité Sepi compilée
*}
function CompileSepiSource(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList; SourceFile: TStrings;
  const DestFileName: TFileName; RootNodeClass: TSepiParseTreeRootNodeClass;
  RootSymbolClass: TSepiSymbolClass; LexerClass: TSepiCustomLexerClass;
  ParserClass: TSepiCustomParserClass): TSepiUnit;
var
  DestFile: TStream;
  RootNode: TSepiParseTreeRootNode;
  Compiler: TSepiUnitCompiler;
begin
  // Silence the compiler warning
  Result := nil;

  DestFile := nil;
  RootNode := nil;
  try
    // Actually compile the source file
    RootNode := RootNodeClass.Create(RootSymbolClass, SepiRoot, Errors);
    try
      ParserClass.Parse(RootNode, LexerClass.Create(Errors,
        SourceFile.Text, Errors.CurrentFileName));
    except
      on Error: ESepiCompilerFatalError do
        raise;
      on Error: Exception do
      begin
        Errors.MakeError(Error.Message, ekFatalError,
          RootNode.FindRightMost.SourcePos);
      end;
    end;

    // Check for errors
    Errors.CheckForErrors;

    // Fetch Sepi unit compiler and unit
    Compiler := RootNode.UnitCompiler;
    Result := Compiler.SepiUnit;

    // Compile and write compiled unit to destination stream
    try
      DestFile := TFileStream.Create(DestFileName, fmCreate);
      Compiler.WriteToStream(DestFile);
    except
      on EStreamError do
        Errors.MakeError(Format(SCantOpenDestFile, [DestFileName]),
          ekFatalError);
      on Error: ESepiCompilerFatalError do
        raise;
      on Error: Exception do
        Errors.MakeError(Error.Message, ekFatalError);
    end;
  finally
    RootNode.Free;
    DestFile.Free;
  end;
end;

{--------------------------}
{ TSepiMemberBuilder class }
{--------------------------}

{*
  Crée un nouveau constructeur de membre
  @param AOwner   Propriétaire du membre à construire
*}
constructor TSepiMemberBuilder.Create(AOwner: TSepiComponent);
begin
  inherited Create;

  FOwner := AOwner;

  if Owner is TSepiType then
  begin
    FOwnerType := TSepiType(Owner);

    if Owner is TSepiRecordType then
      FOwnerRecord := TSepiRecordType(Owner)
    else if Owner is TSepiClass then
      FOwnerClass := TSepiClass(Owner)
    else if Owner is TSepiInterface then
      FOwnerIntf := TSepiInterface(Owner);
  end;
end;

{*
  Produit un message d'erreur
  @param ErrorMsg   Message d'erreur
  @param Kind       Type d'erreur
*}
procedure TSepiMemberBuilder.MakeError(const ErrorMsg: string;
  Kind: TSepiErrorKind = ekError);
begin
  CurrentNode.MakeError(ErrorMsg, Kind);
end;

{*
  Cherche un membre du type englobant d'après son nom
  @param MemberName   Nom du membre
  @return Membre recherché, ou nil si non trouvé
*}
function TSepiMemberBuilder.LookForMember(const MemberName: string): TSepiComponent;
begin
  if OwnerRecord <> nil then
    Result := OwnerRecord.GetComponent(MemberName)
  else if OwnerClass <> nil then
    Result := OwnerClass.LookForMember(MemberName)
  else if OwnerIntf <> nil then
    Result := OwnerIntf.LookForMember(MemberName)
  else
    Result := nil;
end;

{*
  Cherche un membre du type englobant d'après son nom
  En cas d'erreur, produit un message d'erreur.
  @param MemberName   Nom du membre
  @return Membre recherché, ou nil si non trouvé
*}
function TSepiMemberBuilder.LookForMemberOrError(
  const MemberName: string): TSepiComponent;
begin
  Result := LookForMember(MemberName);
  CheckIdentFound(Result, MemberName, CurrentNode);
end;

{----------------------------}
{ TSepiPropertyBuilder class }
{----------------------------}

{*
  Crée un nouveau constructeur de propriété
  @param AOwner   Propriétaire du membre à construire
*}
constructor TSepiPropertyBuilder.Create(AOwner: TSepiComponent);
begin
  inherited Create(AOwner);

  FSignature := TSepiSignature.CreateConstructing(Owner.OwningUnit, OwnerType);
  FSignature.Kind := skProperty;

  FIndex := NoIndex;
  FDefaultValue := NoDefaultValue;
  FStorage.Kind := pskConstant;
  FStorage.Stored := True;
end;

{*
  [@inheritDoc]
*}
destructor TSepiPropertyBuilder.Destroy;
begin
  FSignature.Free;

  inherited;
end;

{*
  Valide un accès via un champ
  @param Field   Champ accesseur
  @return True si le champ est valide, False sinon
*}
function TSepiPropertyBuilder.ValidateFieldAccess(Field: TSepiField): Boolean;
begin
  if Signature.ParamCount > 0 then
  begin
    MakeError(SParametersMismatch);
    Result := False;
  end else if Field.FieldType <> Signature.ReturnType then
  begin
    MakeError(Format(STypeMismatch,
      [Field.FieldType.DisplayName, Signature.ReturnType.DisplayName]));
    Result := False;
  end else
  begin
    Result := True;
  end;
end;

{*
  Valide un accès via une méthode
  @param Method          Méthode accesseur
  @param IsWriteAccess   Indique si c'est l'accès en écriture
  @return True si la méthode est valide, False sinon
*}
function TSepiPropertyBuilder.ValidateMethodAccess(Method: TSepiMethod;
  IsWriteAccess: Boolean): Boolean;
var
  ParamCount, I: Integer;
  Param: TSepiParam;
begin
  ParamCount := Signature.ParamCount;
  if Index <> NoIndex then
    Inc(ParamCount);
  if IsWriteAccess then
    Inc(ParamCount);

  if Method.Signature.ParamCount <> ParamCount then
  begin
    MakeError(SParametersMismatch);
    Result := False;
    Exit;
  end;

  Result := True;

  // Check parameters
  for I := 0 to Signature.ParamCount-1 do
  begin
    if not Method.Signature.Params[I].Equals(Signature.Params[I],
      pcoCompatibility) then
    begin
      Result := False;
      Break;
    end;
  end;

  // Check index
  if Result and (Index <> NoIndex) then
  begin
    Param := Method.Signature.Params[Signature.ParamCount];

    if (Param.Kind in [pkVar, pkOut]) or (Param.ParamType = nil) then
      Result := False;

    if (IndexType <> nil) and
      (not Param.ParamType.CompatibleWith(IndexType)) then
      Result := False;
  end;

  // Check property type
  if Result then
  begin
    if IsWriteAccess then
    begin
      Param := Method.Signature.Params[Method.Signature.ParamCount-1];

      if (Param.Kind in [pkVar, pkOut]) or
        (Param.ParamType <> Signature.ReturnType) then
        Result := False;
    end else
    begin
      if Method.Signature.ReturnType <> Signature.ReturnType then
        Result := False;
    end;
  end;

  // Error message
  if not Result then
    MakeError(SParametersMismatch);
end;

{*
  Valide un accesseur
  @param Access          Accesseur
  @param IsWriteAccess   Indique si c'est l'accès en écriture
  @return True si l'accesseur est valide, False sinon
*}
function TSepiPropertyBuilder.ValidateAccess(Access: TSepiComponent;
  IsWriteAccess: Boolean): Boolean;
begin
  if Access = nil then
    Result := False
  else if Access is TSepiField then
    Result := ValidateFieldAccess(TSepiField(Access))
  else if Access is TSepiMethod then
    Result := ValidateMethodAccess(TSepiMethod(Access), IsWriteAccess)
  else
  begin
    MakeError(SFieldOrMethodRequired);
    Result := False;
  end;
end;

{*
  Teste si un type de propriété peut avoir un default
*}
function TSepiPropertyBuilder.IsTypeValidForDefault(
  PropType: TSepiType): Boolean;
begin
  if PropType is TSepiOrdType then
    Result := True
  else if (PropType is TSepiSetType) and (PropType.Size <= SizeOf(Integer)) then
    Result := True
  else
    Result := False;
end;

{*
  Vérifie que default ou nodefault est valide
  @return True si autorisé, False sinon
*}
function TSepiPropertyBuilder.CheckDefaultAllowed: Boolean;
begin
  if Signature.ParamCount > 0 then
  begin
    MakeError(SScalarPropertyRequired);
    Result := False;
  end else if not IsTypeValidForDefault(Signature.ReturnType) then
  begin
    MakeError(SOrdinalTypeRequired);
    Result := False;
  end else
  begin
    Result := True;
  end;
end;

{*
  Redéfinit une propriété héritée
  @param Node   Noeud utilisé pour produire les erreurs
  @return True en cas de succès, False sinon
*}
function TSepiPropertyBuilder.Redefine(Node: TSepiParseTreeNode): Boolean;
var
  PreviousComponent: TSepiComponent;
  Previous: TSepiProperty;
  I: Integer;
begin
  CurrentNode := Node;

  PreviousComponent := LookForMember(Name);
  if not (PreviousComponent is TSepiProperty) then
  begin
    MakeError(SPropertyNotFoundInBaseClass);
    Signature.ReturnType := Node.SystemUnit.Integer;
    Result := False;
    Exit;
  end;

  Previous := TSepiProperty(PreviousComponent);

  for I := 0 to Previous.Signature.ParamCount-1 do
    TSepiParam.Clone(Signature, Previous.Signature.Params[I]);
  Signature.ReturnType := Previous.Signature.ReturnType;

  FReadAccess := Previous.ReadAccess.Component;
  FWriteAccess := Previous.WriteAccess.Component;
  FIndex := Previous.Index;
  FDefaultValue := Previous.DefaultValue;
  FStorage := Previous.Storage;
  FIsDefault := Previous.IsDefault;

  Result := True;
end;

{*
  Spécifie l'accesseur en lecture
  @param ReadAccessName   Nom de l'accesseur
  @param Node             Noeud utilisé pour produire les erreurs
  @return True en cas de succès, False sinon
*}
function TSepiPropertyBuilder.SetReadAccess(const ReadAccessName: string;
  Node: TSepiParseTreeNode): Boolean;
var
  Access: TSepiComponent;
begin
  CurrentNode := Node;

  Access := LookForMemberOrError(ReadAccessName);
  Result := ValidateAccess(Access, False);

  if Result then
    FReadAccess := Access;
end;

{*
  Spécifie l'accesseur en écriture
  @param ReadAccessName   Nom de l'accesseur
  @param Node             Noeud utilisé pour produire les erreurs
  @return True en cas de succès, False sinon
*}
function TSepiPropertyBuilder.SetWriteAccess(const WriteAccessName: string;
  Node: TSepiParseTreeNode): Boolean;
var
  Access: TSepiComponent;
begin
  CurrentNode := Node;

  Access := LookForMemberOrError(WriteAccessName);
  Result := ValidateAccess(Access, True);

  if Result then
    FWriteAccess := Access;
end;

{*
  Spécifie l'index
  @param Value   Valeur index
  @param Node    Noeud utilisé pour produire les erreurs
  @return True en cas de succès, False sinon
*}
function TSepiPropertyBuilder.SetIndex(
  const Value: ISepiReadableValue; Node: TSepiParseTreeNode): Boolean;
begin
  CurrentNode := Node;

  if not (Value.ValueType is TSepiOrdType) then
  begin
    MakeError(SOrdinalTypeRequired);
    Result := False;
  end else if not Value.IsConstant then
  begin
    MakeError(SConstExpressionRequired);
    Result := False;
  end else
  begin
    FIndexType := TSepiOrdType(Value.ValueType);
    FIndex := FIndexType.ValueAsInteger(Value.ConstValuePtr^);
    Result := True;
  end;
end;

{*
  Spécifie la valeur par défaut
  @param Value   Valeur par défaut
  @param Node    Noeud utilisé pour produire les erreurs
  @return True en cas de succès, False sinon
*}
function TSepiPropertyBuilder.SetDefaultValue(
  const Value: ISepiReadableValue; Node: TSepiParseTreeNode): Boolean;
begin
  CurrentNode := Node;

  if not CheckDefaultAllowed then
  begin
    Result := False;
  end else if not Value.ValueType.Equals(Signature.ReturnType) then
  begin
    MakeError(Format(STypeMismatch,
      [Signature.ReturnType.DisplayName, Value.ValueType.DisplayName]));
    Result := False;
  end else if not Value.IsConstant then
  begin
    MakeError(SConstExpressionRequired);
    Result := False;
  end else
  begin
    FDefaultValue := TSepiOrdType(Value.ValueType).ValueAsInteger(
      Value.ConstValuePtr^);
    Result := True;
  end;
end;

{*
  Spécifie qu'il n'y a pas de valeur par défaut
  @param Node   Noeud utilisé pour produire les erreurs
  @return True en cas de succès, False sinon
*}
function TSepiPropertyBuilder.SetNoDefault(Node: TSepiParseTreeNode): Boolean;
begin
  CurrentNode := Node;

  Result := CheckDefaultAllowed;

  if Result then
    FDefaultValue := NoDefaultValue;
end;

{*
  Spécifie que c'est la propriété par défaut
  @param Node   Noeud utilisé pour produire les erreurs
  @return True en cas de succès, False sinon
*}
function TSepiPropertyBuilder.SetIsDefault(Node: TSepiParseTreeNode): Boolean;
begin
  CurrentNode := Node;

  if IsDefault then
  begin
    MakeError(SDuplicateDefaultDirective);
    Result := False;
  end else if Signature.ParamCount = 0 then
  begin
    MakeError(SArrayPropertyRequired);
    Result := False;
  end else if (OwnerClass <> nil) and
    (OwnerClass.DefaultProperty <> nil) and
    (OwnerClass.DefaultProperty.Owner = Owner) then
  begin
    MakeError(SDuplicateDefaultProperty);
    Result := False;
  end else
  begin
    FIsDefault := True;
    Result := True;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiPropertyBuilder.Build;
begin
  TSepiProperty.Create(Owner, Name, Signature, ReadAccess, WriteAccess,
    Index, DefaultValue, Storage, IsDefault);
end;

{----------------------------------}
{ TSepiCompilerTransientType class }
{----------------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiCompilerTransientType.Load(AOwner: TSepiComponent; Stream: TStream);
begin
  raise EAssertionFailed.Create('Can''t load instance of '+ClassName);
end;

{*
  Crée un type transient du compilateur
  @param AOwner   Propriétaire du type
  @param AName    Nom du type
  @param AKind    Type de type (tkUnknown par défaut)
*}
constructor TSepiCompilerTransientType.Create(AOwner: TSepiComponent;
  const AName: string; AKind: TTypeKind = tkUnknown);
begin
  inherited Create(AOwner, AName, AKind);

  FSize := 4;
end;

{*
  [@inheritDoc]
*}
procedure TSepiCompilerTransientType.Save(Stream: TStream);
begin
  raise EAssertionFailed.Create('Can''t save instance of '+ClassName);
end;

end.

