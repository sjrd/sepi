{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
-------------------------------------------------------------------------------}

{*
  Producteur d'importeur d'unité native dans Sepi
  @author sjrd
  @version 1.0
*}
unit ImporterProducer;

interface

uses
  Windows, SysUtils, Classes, StrUtils, TypInfo, ScUtils, ScStrUtils,
  ScDelphiLanguage, SepiReflectionCore, SepiMembers, SepiOrdTypes,
  SepiArrayTypes, SepiStrTypes, ImporterTemplates, SepiDelphiCompilerConsts,
  ImporterConsts;

type
  {*
    Producteur d'importeur d'unité native dans Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiImporterProducer = class(TObject)
  private
    TemplateDir: TFileName; /// Dossier des templates

    SepiRoot: TSepiRoot;      /// Racine Sepi
    SepiUnit: TSepiUnit;      /// Unité Sepi
    ProducedUnitName: string; /// Surcharge du nom de l'unité produite

    UsesList: TStrings; /// Liste des uses dans l'importeur produit

    TypeCount: Integer;     /// Nombre de types avec RTTI
    MethodCount: Integer;   /// Nombre de méthodes avec adresse
    VariableCount: Integer; /// Nombre de variables
    LazyLoad: Boolean;      /// Indique si l'unité devrait être chargée en lazy

    function ProduceUsesList: string;
    function Produce: string;

    procedure RequireUnit(const UnitName: string);
    function IdentifierFor(ForComponent, FromComponent: TSepiComponent): string;

    procedure HandleType(Template: TTemplate; SepiType: TSepiType);
    procedure HandleRoutine(Template: TTemplate; Routine: TSepiMethod);
    procedure HandleVariable(Template: TTemplate; Variable: TSepiVariable);
    procedure HandleClassType(Template: TTemplate; SepiClass: TSepiClass);

    function PrepareClassMethodTags(SepiClass: TSepiClass): Boolean;
    procedure HandleMethod(DeclTemplate, ImplTemplate: TTemplate;
      SepiClass: TSepiClass; Method: TSepiMethod);

    function ResolvePropertyMethod(Template: TTemplate;
      Method: TSepiMethod; const AClassName: string = ''): string;
    function ResolveOverloadedMethod(Template: TTemplate;
      Method: TSepiMethod; const AClassName: string = ''): string;

    function NextTypeID: Integer;
    function NextMethodID: Integer;
    function NextVariableID: Integer;

    function MakeSignature(Signature: TSepiSignature; From: TSepiComponent;
      const MethodName: string = ''): string;
  public
    constructor Create(ASepiUnit: TSepiUnit;
      const AProducedUnitName: string = ''; ALazyLoad: Boolean = False);
    destructor Destroy; override;

    class function ProduceImporter(SepiUnit: TSepiUnit;
      const ProducedUnitName: string = ''; LazyLoad: Boolean = False): string;
  end;

function ProduceImporter(SepiUnit: TSepiUnit;
  const ProducedUnitName: string = ''; LazyLoad: Boolean = False): string;

implementation

const // don't localize
  MainTemplateFileName = 'SepiImports.pas';
  ClassDeclTemplateFileName = 'ClassDecl.pas';
  ClassImplTemplateFileName = 'ClassImpl.pas';
  MethodTemplateFileName = 'Method.pas';

  UnitNameParam = 'UnitName';
  UnitName2Param = 'UnitName2';
  UsesListParam = 'UsesList';
  TypeCountParam = 'TypeCount';
  MethodCountParam = 'MethodCount';
  VariableCountParam = 'VariableCount';
  LazyLoadParam = 'LazyLoad';
  TypeDeclParam = 'TypeDecl';
  ImportClassesDeclsParam = 'ImportClassesDecls';
  ImportClassesImplsParam = 'ImportClassesImpls';
  OverloadedsParam = 'Overloadeds';
  InitTypeInfoArrayParam = 'InitTypeInfoArray';
  InitMethodAddressesParam = 'InitMethodAddresses';
  InitVarAddressesParam = 'InitVarAddresses';

  StaticAssertionsParam = 'StaticAssertions';
  DynamicAssertionsParam = 'DynamicAssertions';

  ClassNameParam = 'ClassName';
  DashesParam = 'Dashes';
  MembersParam = 'Members';

  SignatureParam = 'Signature';
  StatementsParam = 'Statements';

  CRLF = #13#10;

{-----------------}
{ Global routines }
{-----------------}

{*
  Produit un importeur pour une unité Sepi
  @param SepiUnit           Unité Sepi
  @param ProducedUnitName   Surcharge du nom de l'unité produite
  @return Code de l'importeur
*}
function ProduceImporter(SepiUnit: TSepiUnit;
  const ProducedUnitName: string = ''; LazyLoad: Boolean = False): string;
begin
  Result := TSepiImporterProducer.ProduceImporter(
    SepiUnit, ProducedUnitName, LazyLoad);
end;

{-----------------------------}
{ TSepiImporterProducer class }
{-----------------------------}

{*
  Crée un producteur d'importeur d'unité Sepi
  @param ASepiUnit           Unité Sepi
  @param AProducedUnitName   Surcharge du nom de l'unité produite
  @param ALazyLoad           L'unité Sepi sera chargée en lazy-load
*}
constructor TSepiImporterProducer.Create(ASepiUnit: TSepiUnit;
  const AProducedUnitName: string = ''; ALazyLoad: Boolean = False);
begin
  inherited Create;

  TemplateDir := Dir + DefaultTemplatesDir;

  SepiUnit := ASepiUnit;
  SepiRoot := SepiUnit.Root;

  if AProducedUnitName = '' then
    ProducedUnitName := SepiUnit.Name
  else
    ProducedUnitName := AProducedUnitName;

  UsesList := TStringList.Create;
  with TStringList(UsesList) do
  begin
    CaseSensitive := False;
    Sorted := True;
    Duplicates := dupIgnore;
  end;

  TypeCount := 0;
  MethodCount := 0;
  LazyLoad := ALazyLoad;
end;

{*
  [@inheritDoc]
*}
destructor TSepiImporterProducer.Destroy;
begin
  UsesList.Free;
  inherited;
end;

{*
  Produit la liste des uses
  @return Liste des uses
*}
function TSepiImporterProducer.ProduceUsesList: string;
const
  InitUses: array[0..5] of string = (
    'Windows', 'SysUtils', 'Classes', 'TypInfo', 'SepiReflectionCore',
    'SepiMembers'
  );
var
  List: TStrings;
  I: Integer;
  UnitName: string;
begin
  List := TStringList.Create;
  try
    TStringList(List).CaseSensitive := False;

    // Make list - in reverse order

    List.Add(SepiUnit.Name);

    for I := SepiUnit.UsedUnitCount-1 downto 0 do
    begin
      UnitName := SepiUnit.UsedUnits[I].Name;

      if UsesList.IndexOf(UnitName) >= 0 then
        List.Add(UnitName);
    end;

    for I := High(InitUses) downto Low(InitUses) do
      if List.IndexOf(InitUses[I]) < 0 then
        List.Add(InitUses[I]);

    // Convert the list to a string

    Result := '';
    for I := List.Count-1 downto 0 do
      Result := Result + ', ' + List[I];
    Result[1] := ' ';
    Result := Result + ';';
    Result := WrapText(Result, CRLF+'  ', [' '], 80);
  finally
    List.Free;
  end;
end;

{*
  Méthode de production principale
  @return Code de l'importeur
*}
function TSepiImporterProducer.Produce: string;
var
  Template: TTemplate;
  I: Integer;
  Component: TSepiComponent;
begin
  Template := TTemplate.Create(TemplateDir+MainTemplateFileName);
  try
    // Initialize template
    with Template do
    begin
      SetParam(UnitNameParam, SepiUnit.Name);
      SetParam(UnitName2Param, ProducedUnitName);
      SetParam(TypeDeclParam, '');
      SetParam(ImportClassesDeclsParam, '');
      SetParam(ImportClassesImplsParam, '');
      SetParam(OverloadedsParam, '');
      SetParam(InitTypeInfoArrayParam, '');
      SetParam(InitMethodAddressesParam, '');
      SetParam(InitVarAddressesParam, '');
      SetParam(StaticAssertionsParam, '');
      SetParam(DynamicAssertionsParam, '');
    end;

    // Initialize uses list
    UsesList.Clear;

    // Iterate through each unit member
    for I := 0 to SepiUnit.ChildCount-1 do
    begin
      Component := SepiUnit.Children[I];

      // Produce Component importer
      if Component is TSepiType then
        HandleType(Template, TSepiType(Component))
      else if Component is TSepiMethod then
        HandleRoutine(Template, TSepiMethod(Component))
      else if Component is TSepiVariable then
        HandleVariable(Template, TSepiVariable(Component));
    end;

    // Finalize template
    with Template do
    begin
      SetParam(UsesListParam, ProduceUsesList);

      if TypeCount = 0 then
        SetParam(TypeCountParam, 1)
      else
        SetParam(TypeCountParam, TypeCount);

      if MethodCount = 0 then
        SetParam(MethodCountParam, 1)
      else
        SetParam(MethodCountParam, MethodCount);

      if VariableCount = 0 then
        SetParam(VariableCountParam, 1)
      else
        SetParam(VariableCountParam, VariableCount);

      SetParam(LazyLoadParam, LazyLoad);
    end;

    // Process template
    Result := Template.Process;
  finally
    Template.Free;
  end;
end;

{*
  Requiert qu'une unité soit dans les uses
  @param UnitName   Nom de l'unité à mettre dans les uses
*}
procedure TSepiImporterProducer.RequireUnit(const UnitName: string);
begin
  if not AnsiSameText(UnitName, SystemUnitName) then
    UsesList.Add(UnitName);
end;

{*
  Produit un identificateur pour un composant donné depuis un autre composant
  En plus de cela, cette méthode ajoute les uses nécessaires.
  @param ForComponent    Composant destination
  @param FromComponent   Composant à partir duquel le référencer
  @return Identificateur pour ce composant
*}
function TSepiImporterProducer.IdentifierFor(
  ForComponent, FromComponent: TSepiComponent): string;
var
  First: TSepiComponent;
begin
  Result := '';

  if ForComponent.Visibility in [mvStrictPrivate, mvPrivate] then
    Exit;
  if not (FromComponent is TSepiInheritableContainerType) and
    (ForComponent.Visibility in [mvStrictProtected, mvProtected]) then
    Exit;

  Result := ForComponent.GetShorterNameFrom(FromComponent);
  if Result = '' then
    Exit;

  First := FromComponent.LookFor(GetFirstToken(Result, '.'));

  // Artificially qualify inner types
  while (First.Owner <> First.OwningUnit) and not (First is TSepiMember) and
    not (First is TSepiUnit) do
  begin
    First := First.Owner;
    Result := First.Name + '.' + Result;
  end;

  if Pos('$', Result) > 0 then
  begin
    // Anonymous components can never be accessed in a real Delphi code
    Result := '';
    Exit;
  end;

  Assert(FromComponent.LookFor(Result) = ForComponent);
  RequireUnit(First.OwningUnit.Name);
end;

{*
  Teste si un type est un ensemble spécial qui n'a pas de RTTI en Delphi
  @param SepiType   Type à tester
  @return True si c'est un type ensemble spécial sans RTTI, False sinon
*}
function IsSpecialSetWithoutTypeInfo(SepiType: TSepiType): Boolean;
var
  SetType: TSepiSetType;
begin
  Result := False;
  if not (SepiType is TSepiSetType) then
    Exit;

  SetType := TSepiSetType(SepiType);

  if SetType.CompType.Name[1] <> '$' then
    Exit;

  if (SetType.CompType is TSepiEnumType) and
    (TSepiEnumType(SetType.CompType).BaseType = SetType.CompType) then
    Exit;

  Result := True;
end;

{*
  Produit l'importation d'un type
  @param Template   Template de l'importeur d'unité
  @param SepiType   Type à importer
*}
procedure TSepiImporterProducer.HandleType(Template: TTemplate;
  SepiType: TSepiType);
const
  CheckSizeOfStatement = CRLF+
    '{$IF SizeOf(%s) <> %d}'+CRLF+
    '  {$MESSAGE WARN ''Le type %0:s n''''a pas la taille calculée par '+
      'Sepi''}'+CRLF;
  CheckAlignmentStatement = CRLF+
    'type'+CRLF+
    '  TCheckAlignmentFor%0:s = record'+CRLF+
    '    Dummy: Byte;'+CRLF+
    '    Field: %1:s;'+CRLF+
    '  end;'+CRLF+CRLF+
    '{$IF SizeOf(TCheckAlignmentFor%0:s) <> (%2:d + %3:d)}'+CRLF+
    '  {$MESSAGE WARN ''Le type %1:s n''''a pas l''''alignement calculé par '+
      'Sepi''}'+CRLF+
    '{$IFEND}'+CRLF;
  NoTypeInfo = -1;
  CantFindTypeInfo = -2;
  SetTypeInfoStatement =
    '  TypeInfoArray[%d] := TypeInfo(%s);'+CRLF;
var
  FullName: string;
  HasSizeOfAssertion: Boolean;
  I, AddToAlignment: Integer;
begin
  // Find a name that access the type from the Delphi code
  FullName := IdentifierFor(SepiType, SepiType.OwningUnit);

  // If that was impossible, we are stuck anyway
  if FullName = '' then
  begin
    if SepiType.TypeInfo = nil then
      SepiType.Tag := NoTypeInfo
    else
      SepiType.Tag := CantFindTypeInfo;

    Exit;
  end;

  // Check Size was computed correctly for sets, records and arrays
  if (SepiType is TSepiSetType) or (SepiType is TSepiRecordType) or
    (SepiType is TSepiStaticArrayType) then
  begin
    HasSizeOfAssertion := True;
    Template.AddToParam(StaticAssertionsParam,
      Format(CheckSizeOfStatement, [FullName, SepiType.Size]));
  end else
    HasSizeOfAssertion := False;

  // Check that Alignment was computed correctly
  if SepiType.Size < $1000000 then
  begin
    if HasSizeOfAssertion then
      Template.AddToParam(StaticAssertionsParam, '{$ELSE}'+CRLF);

    AddToAlignment := SepiType.Size;
    SepiType.AlignOffset(AddToAlignment);

    Template.AddToParam(StaticAssertionsParam,
      Format(CheckAlignmentStatement, [AnsiReplaceStr(FullName, '.', ''),
      FullName, SepiType.Alignment, AddToAlignment]));

    if HasSizeOfAssertion then
      Template.AddToParam(StaticAssertionsParam, CRLF);
  end;

  if HasSizeOfAssertion then
    Template.AddToParam(StaticAssertionsParam, '{$IFEND}'+CRLF);

  // Find its TypeInfo
  with SepiType do
  begin
    if TypeInfo = nil then
      Tag := NoTypeInfo
    else if IsSpecialSetWithoutTypeInfo(SepiType) then
      Tag := CantFindTypeInfo
    else
    begin
      Tag := NextTypeID;
      Template.AddToParam(InitTypeInfoArrayParam,
        Format(SetTypeInfoStatement, [Tag, FullName]));
    end;
  end;

  // Handle classes
  if SepiType is TSepiClass then
    HandleClassType(Template, TSepiClass(SepiType));

  // Handle inner types
  for I := 0 to SepiType.ChildCount-1 do
  begin
    if SepiType.Children[I] is TSepiType then
      HandleType(Template, TSepiType(SepiType.Children[I]));
  end;
end;

{*
  Produit l'importation d'une routine
  @param Template   Template de l'importeur d'unité
  @param Routine    Routine à importer
*}
procedure TSepiImporterProducer.HandleRoutine(Template: TTemplate;
  Routine: TSepiMethod);
const
  SetMethodAddressStatement =
    '  MethodAddresses[%d] := @%s;'+CRLF;
var
  StrAddress: string;
begin
  with Routine do
  begin
    // Compiler magic routines must not be imported
    if Name[1] = '_' then
      Exit;

    Tag := NextMethodID;

    if IsOverloaded then
      StrAddress := ResolveOverloadedMethod(Template, Routine)
    else
      StrAddress := Name;

    Template.AddToParam(InitMethodAddressesParam,
      Format(SetMethodAddressStatement, [Tag, StrAddress]));
  end;
end;

{*
  Produit l'importation d'une routine
  @param Template   Template de l'importeur d'unité
  @param Variable   Variable à importer
*}
procedure TSepiImporterProducer.HandleVariable(Template: TTemplate;
  Variable: TSepiVariable);
const
  SetVarAddressStatement =
    '  VarAddresses[%d] := @%s;'+CRLF;
var
  StrAddress: string;
begin
  with Variable do
  begin
    Tag := NextVariableID;

    StrAddress := Name;
    if VarType is TSepiMethodRefType then
      StrAddress := '@'+StrAddress;

    Template.AddToParam(InitVarAddressesParam,
      Format(SetVarAddressStatement, [Tag, StrAddress]));
  end;
end;

{*
  Produit l'importation d'un type classe
  @param Template    Template de l'importeur d'unité
  @param SepiClass   Classe à importer
*}
procedure TSepiImporterProducer.HandleClassType(Template: TTemplate;
  SepiClass: TSepiClass);
const
  CheckInstanceSizeStatement =
    '  CheckInstanceSize(%s, %d, %d);'+CRLF;
  InitMethAddrCallStatement =
    '  TSepiImports%s.InitMethodAddresses;'+CRLF;
var
  I: Integer;
  Component: TSepiComponent;
  DeclTemplate, ImplTemplate: TTemplate;
begin
  Template.AddToParam(DynamicAssertionsParam,
    Format(CheckInstanceSizeStatement,
      [SepiClass.Name, SepiClass.InstSize, SepiClass.Parent.InstSize]));

  // If returns False, no import must be done for this class
  if not PrepareClassMethodTags(SepiClass) then
    Exit;

  with SepiClass do
  begin
    Template.SetParam(TypeDeclParam, CRLF+'type');
    Template.AddToParam(InitMethodAddressesParam,
      Format(InitMethAddrCallStatement, [Name]));

    ImplTemplate := nil;

    DeclTemplate := TTemplate.Create(TemplateDir+ClassDeclTemplateFileName);
    try
      ImplTemplate := TTemplate.Create(TemplateDir+ClassImplTemplateFileName);

      // Initialize import class declaration template
      with DeclTemplate do
      begin
        SetParam(ClassNameParam, Name);
        SetParam(MembersParam, '');
      end;

      // Initialize import class implementation template
      with ImplTemplate do
      begin
        SetParam(ClassNameParam, Name);
        SetParam(DashesParam, StringOfChar('-', Length(Name)));
        SetParam(OverloadedsParam, '');
        SetParam(InitMethodAddressesParam, '');
      end;

      // Members
      for I := 0 to ChildCount-1 do
      begin
        Component := Children[I];
        if (Component is TSepiMethod) and (Component.Tag >= 0) then
          HandleMethod(DeclTemplate, ImplTemplate,
            SepiClass, TSepiMethod(Component));
      end;
    finally
      if Assigned(ImplTemplate) then
      begin
        Template.AddToParam(ImportClassesImplsParam, ImplTemplate.Process);
        ImplTemplate.Free;
      end;

      Template.AddToParam(ImportClassesDeclsParam, DeclTemplate.Process);
      DeclTemplate.Free;
    end;
  end;
end;

{*
  Prépare les tags des méthodes d'une classe
  @param SepiClass   Classe Sepi
  @return True s'il faut prévoir une importation pour la classe, False sinon
*}
function TSepiImporterProducer.PrepareClassMethodTags(
  SepiClass: TSepiClass): Boolean;
const
  DontGetCode = -1;
var
  I, J: Integer;
  Component: TSepiComponent;
  Method: TSepiMethod;
  Prop: TSepiProperty;
begin
  Result := False;

  with SepiClass do
  begin
    // Initialize tags
    for I := 0 to ChildCount-1 do
    begin
      Component := Children[I];
      if Component is TSepiMethod then
      begin
        Method := TSepiMethod(Component);
        with Method do
        begin
          Tag := DontGetCode;

          { For virtual, dynamic, or message methods, Sepi reflection classes
            will read method code automatically.
            For static (strict) private methods which are access method of a
            non-private property (without index specifier), we can access its
            code through this property.
            For other (strict) private methods, we have no way at all to gain
            access to its code. The only case which we are tricked in is: when
            there are index properties which access the method, but no property
            doing it without index. This could be improved. }

          if LinkKind <> mlkStatic then
            Continue;

          if Visibility in [mvStrictPrivate, mvPrivate] then
          begin
            // Private method
            for J := 0 to SepiClass.ChildCount-1 do
            begin
              Component := SepiClass.Children[J];
              if not (Component is TSepiProperty) then
                Continue;
              Prop := TSepiProperty(Component);
              if Prop.Visibility in [mvStrictPrivate, mvPrivate] then
                Continue;
              if Prop.Index <> NoIndex then
                Continue;

              if (Prop.ReadAccess.Component = Method) or
                (Prop.WriteAccess.Component = Method) then
              begin
                Tag := NextMethodID;
                Result := True;
                Break;
              end;
            end;
          end else
          begin
            // Protected, public or published method
            Tag := NextMethodID;
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

{*
  Produit l'importation d'une méthode de classe
  @param DeclTemplate   Template de la déclaration de la classe
  @param ImplTemplate   Template de l'implémentation de la classe
  @param SepiClass      Classe Sepi
  @param Method         Méthode à importer
*}
procedure TSepiImporterProducer.HandleMethod(
  DeclTemplate, ImplTemplate: TTemplate; SepiClass: TSepiClass;
  Method: TSepiMethod);
const
  SetMethodAddressStatement =
    '  MethodAddresses[%d] := @%s.%s;'+CRLF;
var
  ImportClassName, StrAddress: string;
begin
  ImportClassName := 'TSepiImports' + SepiClass.Name;

  with Method do
  begin
    if Visibility in [mvStrictPrivate, mvPrivate] then
    begin
      // Resolve using a property
      StrAddress := ResolvePropertyMethod(ImplTemplate,
        Method, ImportClassName);
      DeclTemplate.AddToParam(MembersParam, Format('    %s;'+CRLF,
        [MakeSignature(Signature, Method, StrAddress)]));
    end else if IsOverloaded then
    begin
      // Resolve overloaded
      StrAddress := ResolveOverloadedMethod(ImplTemplate,
        Method, ImportClassName);
      DeclTemplate.AddToParam(MembersParam, Format('    %s;'+CRLF,
        [MakeSignature(Signature, Method, StrAddress)]));
    end else
    begin
      // Direct access
      StrAddress := Name;
    end;

    // Statement
    ImplTemplate.AddToParam(InitMethodAddressesParam, Format(
      SetMethodAddressStatement, [Tag, ImportClassName, StrAddress]));
  end;
end;

{*
  Résoud l'importation d'une méthode via une propriété
  @param Template     Template de l'importeur d'unité
  @param Method       Méthode à résoudre
  @param AClassName   Nom de la classe de la méthode
  @return Nom d'import de la méthode
*}
function TSepiImporterProducer.ResolvePropertyMethod(Template: TTemplate;
  Method: TSepiMethod; const AClassName: string): string;
var
  I: Integer;
  Prop: TSepiProperty;
  ResolveTpl: TTemplate;
  IsReadAccess: Boolean;
  Statement, AMethodName: string;
begin
  Prop := nil;
  IsReadAccess := False;

  // Find appropriate property
  with Method.Owner do
  begin
    for I := 0 to ChildCount-1 do
    begin
      if Children[I] is TSepiProperty then
      begin
        Prop := TSepiProperty(Children[I]);

        if Prop.Visibility in [mvStrictPrivate, mvPrivate] then
          Continue;
        if Prop.Index <> NoIndex then
          Continue;

        if Prop.ReadAccess.Component = Method then
        begin
          IsReadAccess := True;
          Break;
        end else if Prop.WriteAccess.Component = Method then
        begin
          IsReadAccess := False;
          Break;
        end;
      end;
    end;
  end;

  // Produce resolver
  ResolveTpl := TTemplate.Create(TemplateDir+MethodTemplateFileName);
  try
    with Method do
    begin
      if IsOverloaded then
        Result := Format('%s_%d', [Overloaded.Name, OverloadIndex])
      else
        Result := Name;

      if Signature.GetParam(Prop.Name) = nil then
        Statement := Prop.Name
      else
        Statement := 'Self.' + Prop.Name;

      if Prop.Signature.ParamCount > 0 then
      begin
        Statement := Statement + '[';
        for I := 0 to Prop.Signature.ParamCount-1 do
          Statement := Statement + Signature.Params[I].Name + ', ';
        SetLength(Statement, Length(Statement)-1);
        Statement[Length(Statement)] := ']';
      end;

      if IsReadAccess then
        Statement := '  Result := ' + Statement + ';' + CRLF
      else
        Statement := '  ' + Statement + ' := ' +
          Signature.Params[Signature.ParamCount-1].Name + ';' + CRLF;

      AMethodName := AClassName + '.' + Result;

      with ResolveTpl do
      begin
        SetParam(SignatureParam,
          MakeSignature(Signature, Method, AMethodName));
        SetParam(StatementsParam, Statement);
      end;
    end;

    Template.AddToParam(OverloadedsParam, ResolveTpl.Process);
  finally
    ResolveTpl.Free;
  end;
end;

{*
  Résoud l'importation d'une méthode overload
  @param Template     Template de l'importeur d'unité
  @param Method       Méthode à résoudre
  @param AClassName   Nom de la classe de la méthode (vide pour une routine)
  @return Nom d'import de la méthode
*}
function TSepiImporterProducer.ResolveOverloadedMethod(Template: TTemplate;
  Method: TSepiMethod; const AClassName: string = ''): string;
var
  ResolveTpl: TTemplate;
  IsFunction: Boolean;
  Statement, AMethodName: string;
  I: Integer;
begin
  ResolveTpl := TTemplate.Create(TemplateDir+MethodTemplateFileName);
  try
    with Method do
    begin
      Result := Format('%s_%d', [Overloaded.Name, OverloadIndex]);
      IsFunction := Signature.ReturnType <> nil;

      if Signature.GetParam(Overloaded.Name) = nil then
        Statement := Overloaded.Name
      else if Signature.SelfParam = nil then
        Statement := Overloaded.GetFullName
      else
        Statement := 'Self.' + Overloaded.Name;

      if Signature.ParamCount > 0 then
      begin
        Statement := Statement + '(';
        for I := 0 to Signature.ParamCount-1 do
          Statement := Statement + Signature.Params[I].Name + ', ';
        SetLength(Statement, Length(Statement)-1);
        Statement[Length(Statement)] := ')';
      end;

      if IsFunction then
        Statement := '  Result := ' + Statement + ';' + CRLF
      else
        Statement := '  ' + Statement + ';' + CRLF;

      if AClassName = '' then
        AMethodName := Result
      else
        AMethodName := AClassName + '.' + Result;

      with ResolveTpl do
      begin
        SetParam(SignatureParam,
          MakeSignature(Signature, Method, AMethodName));
        SetParam(StatementsParam, Statement);
      end;
    end;

    Template.AddToParam(OverloadedsParam, ResolveTpl.Process);
  finally
    ResolveTpl.Free;
  end;
end;

{*
  Détermine l'ID de type suivant
  @return Prochain ID de type
*}
function TSepiImporterProducer.NextTypeID: Integer;
begin
  Result := TypeCount;
  Inc(TypeCount);
end;

{*
  Détermine l'ID de méthode suivant
  @return Prochain ID de méthode
*}
function TSepiImporterProducer.NextMethodID: Integer;
begin
  Result := MethodCount;
  Inc(MethodCount);
end;

{*
  Détermine l'ID de variable suivant
  @return Prochain ID de variable
*}
function TSepiImporterProducer.NextVariableID: Integer;
begin
  Result := VariableCount;
  Inc(VariableCount);
end;

{*
  Produit une représentation chaîne d'une signature
  @param Signature    Signature
  @param From         Component depuis lequel on regarde
  @param MethodName   Nom de la méthode (défaut = '')
  @return Représentation chaîne de la signature
*}
function TSepiImporterProducer.MakeSignature(Signature: TSepiSignature;
  From: TSepiComponent; const MethodName: string = ''): string;
var
  I, Index: Integer;
  Param: TSepiParam;
begin
  // Method kind
  Result := SignatureKindStrings[Signature.Kind];
  if AnsiStartsStr('static ', Result) or AnsiStartsStr('object ', Result) then
    Delete(Result, 1, 7);

  // Method name
  if MethodName <> '' then
    Result := Result + ' ' + MethodName;

  // Parameters
  if Signature.ParamCount > 0 then
  begin
    Index := Length(Result)+1;

    for I := 0 to Signature.ParamCount-1 do
    begin
      Param := Signature.Params[I];

      // Param kind
      Result := Result + ' ';
      case Param.Kind of
        pkVar: Result := Result + 'var ';
        pkConst: Result := Result + 'const ';
        pkOut: Result := Result + 'out ';
      end;

      // Param name
      Result := Result + Param.Name;

      // Param type
      if not Param.IsUntyped then
      begin
        Result := Result + ': ';

        if not Param.OpenArray then
          Result := Result + IdentifierFor(Param.ParamType, From)
        else if (Param.ParamType as TSepiOpenArrayType).IsArrayOfConst then
          Result := Result + 'array of const'
        else
          Result := Result + 'array of '+IdentifierFor(Param.ElementType, From);
      end;

      Result := Result + ';';
    end;

    if Signature.Kind = skProperty then
    begin
      Result[Index] := '[';
      Result[Length(Result)] := ']';
    end else
    begin
      Result[Index] := '(';
      Result[Length(Result)] := ')';
    end;
  end;

  // Result type
  if Signature.ReturnType <> nil then
    Result := Result + ': ' + IdentifierFor(Signature.ReturnType, From);

  // Calling convention
  if Signature.CallingConvention <> ccRegister then
    Result := Result + '; ' +
      CallingConventionStrings[Signature.CallingConvention];
end;

{*
  Produit un importeur pour une unité Sepi
  @param SepiUnit           Unité Sepi
  @param ProducedUnitName   Surcharge du nom de l'unité produite
  @param LazyLoad           L'unité Sepi sera chargée en lazy-load
  @return Code de l'importeur
*}
class function TSepiImporterProducer.ProduceImporter(SepiUnit: TSepiUnit;
  const ProducedUnitName: string = ''; LazyLoad: Boolean = False): string;
begin
  with Create(SepiUnit, ProducedUnitName, LazyLoad) do
  try
    Result := Produce;
  finally
    Free;
  end;
end;

end.

