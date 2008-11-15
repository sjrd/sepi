{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of the SCL (Sepi Code Library), which is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------}

{*
  Producteur d'importeur d'unité native dans Sepi
  @author sjrd
  @version 1.0
*}
unit ImporterProducer;

interface

uses
  Windows, SysUtils, Classes, StrUtils, TypInfo, ScUtils, ScDelphiLanguage,
  SepiReflectionCore, SepiMembers, SepiOrdTypes, SepiArrayTypes, SepiStrTypes,
  ImporterTemplates, SepiDelphiCompilerConsts, ImporterConsts;

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

    TypeCount: Integer;   /// Nombre de types avec RTTI
    MethodCount: Integer; /// Nombre de méthodes avec adresse
    LazyLoad: Boolean;    /// Indique si l'unité devrait être chargée en lazy

    function Produce: string;

    procedure RequireUnit(const UnitName: string);
    function IdentifierFor(ForMeta, FromMeta: TSepiMeta): string;

    procedure HandleType(Template: TTemplate; SepiType: TSepiType);
    procedure HandleRoutine(Template: TTemplate; Routine: TSepiMethod);
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

    function MakeSignature(Signature: TSepiSignature; From: TSepiMeta;
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
  LazyLoadParam = 'LazyLoad';
  TypeDeclParam = 'TypeDecl';
  ImportClassesDeclsParam = 'ImportClassesDecls';
  ImportClassesImplsParam = 'ImportClassesImpls';
  OverloadedsParam = 'Overloadeds';
  InitTypeInfoArrayParam = 'InitTypeInfoArray';
  InitMethodAddressesParam = 'InitMethodAddresses';

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
  TStringList(UsesList).CaseSensitive := False;

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
  Méthode de production principale
  @return Code de l'importeur
*}
function TSepiImporterProducer.Produce: string;
var
  Template: TTemplate;
  I: Integer;
  Meta: TSepiMeta;
  StrUsesList: string;
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
    end;

    // Initialize uses list
    UsesList.Clear;
    UsesList.Add('Windows');
    UsesList.Add('SysUtils');
    UsesList.Add('Classes');
    UsesList.Add('TypInfo');
    UsesList.Add('SepiReflectionCore');
    UsesList.Add('SepiMembers');

    // Iterate through each unit member
    for I := 0 to SepiUnit.ChildCount-1 do
    begin
      Meta := SepiUnit.Children[I];

      // Produce meta importer
      if Meta is TSepiType then
        HandleType(Template, TSepiType(Meta))
      else if Meta is TSepiMethod then
        HandleRoutine(Template, TSepiMethod(Meta));
    end;

    // Make uses list as string
    RequireUnit(SepiUnit.Name);
    StrUsesList := '';
    for I := 0 to UsesList.Count-1 do
      StrUsesList := StrUsesList + ', ' + UsesList[I];
    StrUsesList[1] := ' ';
    StrUsesList := StrUsesList + ';';
    StrUsesList := WrapText(StrUsesList, CRLF+'  ', [' '], 80);

    // Finalize template
    with Template do
    begin
      SetParam(UsesListParam, StrUsesList);

      if TypeCount = 0 then
        SetParam(TypeCountParam, 1)
      else
        SetParam(TypeCountParam, TypeCount);

      if MethodCount = 0 then
        SetParam(MethodCountParam, 1)
      else
        SetParam(MethodCountParam, MethodCount);

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
var
  Index: Integer;
begin
  if AnsiSameText(UnitName, SystemUnitName) then
    Exit;

  Index := UsesList.IndexOf(UnitName);
  if Index >= 0 then
    UsesList.Delete(Index);

  UsesList.Add(UnitName);
end;

{*
  Produit un identificateur pour un meta donné à partir d'un autre meta
  En plus de cela, cette méthode ajoute les uses nécessaires.
  @param ForMeta    Meta destination
  @param FromMeta   Meta à partir duquel le référencer
  @return Identificateur pour ce meta
*}
function TSepiImporterProducer.IdentifierFor(
  ForMeta, FromMeta: TSepiMeta): string;
begin
  Result := ForMeta.GetShorterNameFrom(FromMeta);
  RequireUnit(ForMeta.OwningUnit.Name);
end;

{*
  Produit l'importation d'un type
  @param Template   Template de l'importeur d'unité
  @param SepiType   Type à importer
*}
procedure TSepiImporterProducer.HandleType(Template: TTemplate;
  SepiType: TSepiType);
const
  NoTypeInfo = -1;
  CantFindTypeInfo = -2;
  SetTypeInfoStatement =
    '  TypeInfoArray[%d] := TypeInfo(%s);'+CRLF;
begin
  with SepiType do
  begin
    if TypeInfo = nil then
      Tag := NoTypeInfo
    else if Name[1] = '$' then
      Tag := CantFindTypeInfo
    else
    begin
      Tag := NextTypeID;
      Template.AddToParam(InitTypeInfoArrayParam,
        Format(SetTypeInfoStatement, [Tag, Name]));
    end;

    if SepiType is TSepiClass then
      HandleClassType(Template, TSepiClass(SepiType));
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
  Produit l'importation d'un type classe
  @param Template    Template de l'importeur d'unité
  @param SepiClass   Classe à importer
*}
procedure TSepiImporterProducer.HandleClassType(Template: TTemplate;
  SepiClass: TSepiClass);
const
  InitMethAddrCallStatement =
    '  TSepiImports%s.InitMethodAddresses;'+CRLF;
var
  I: Integer;
  Meta: TSepiMeta;
  DeclTemplate, ImplTemplate: TTemplate;
begin
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
        Meta := Children[I];
        if (Meta is TSepiMethod) and (Meta.Tag >= 0) then
          HandleMethod(DeclTemplate, ImplTemplate,
            SepiClass, TSepiMethod(Meta));
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
  Meta: TSepiMeta;
  Method: TSepiMethod;
  Prop: TSepiProperty;
begin
  Result := False;

  with SepiClass do
  begin
    // Initialize tags
    for I := 0 to ChildCount-1 do
    begin
      Meta := Children[I];
      if Meta is TSepiMethod then
      begin
        Method := TSepiMethod(Meta);
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
              Meta := SepiClass.Children[J];
              if not (Meta is TSepiProperty) then
                Continue;
              Prop := TSepiProperty(Meta);
              if Prop.Visibility in [mvStrictPrivate, mvPrivate] then
                Continue;
              if Prop.Index <> NoIndex then
                Continue;

              if (Prop.ReadAccess.Meta = Method) or
                (Prop.WriteAccess.Meta = Method) then
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

        if Prop.ReadAccess.Meta = Method then
        begin
          IsReadAccess := True;
          Break;
        end else if Prop.WriteAccess.Meta = Method then
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
          Statement := Statement + Prop.Signature.Params[I].Name + ', ';
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
  Produit une représentation chaîne d'une signature
  @param Signature    Signature
  @param From         Meta depuis lequel on regarde
  @param MethodName   Nom de la méthode (défaut = '')
  @return Représentation chaîne de la signature
*}
function TSepiImporterProducer.MakeSignature(Signature: TSepiSignature;
  From: TSepiMeta; const MethodName: string = ''): string;
var
  I, Index: Integer;
  Param: TSepiParam;
begin
  // Method kind
  Result := MethodKindStrings[Signature.Kind];
  if AnsiStartsStr('unit ', Result) then
    Delete(Result, 1, 5);

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
      if (Param.ParamType = nil) and Param.OpenArray then
        Result := Result + ': array of const;'
      else if Param.ParamType <> nil then
      begin
        if Param.OpenArray then
          Result := Result + ': array of '
        else
          Result := Result + ': ';

        Result := Result + IdentifierFor(Param.ParamType, From) + ';';
      end else
        Result := Result + ';';
    end;

    if Signature.Kind = mkProperty then
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
      CallingConventionNames[Signature.CallingConvention];
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

