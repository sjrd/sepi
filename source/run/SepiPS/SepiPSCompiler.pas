{*
  Compilateur d'unité Sepi-PS
  @author sjrd
  @version 1.0
*}
unit SepiPSCompiler;

interface

uses
  Classes, SepiReflectionCore;

function SepiPSCompile(const Source: string; Output: TStream;
  SepiUnit: TSepiUnit; Errors: TStrings;
  ExportUnexistingProcs: Boolean = True): Boolean; overload;
function SepiPSCompile(const Source: string; Output: TStream;
  const UnitName: string; Errors: TStrings;
  const OnLoadUnit: TSepiLoadUnitEvent = nil): Boolean; overload;

resourcestring
  sDeclDiffersFromPrevious = 'La déclaration diffère de la précédente';
  sCantExportProc = 'Impossible d''exporter la routine %s';

implementation

uses
  Windows, SysUtils, TypInfo, ScUtils, ScStrUtils, ScDelphiLanguage, uPSUtils,
  uPSCompiler, SepiMembers, SepiPSCompileTime, SepiPSLoader;

type
  /// Pointeur vers TCompilingContext
  PCompilingContext = ^TCompilingContext;

  {*
    Contexte de compilation
    @author sjrd
    @version 1.0
  *}
  TCompilingContext = record
    Root: TSepiRoot;                /// Racine Sepi
    SepiUnit: TSepiUnit;            /// Unité Sepi à compiler
    ExportUnexistingProcs: Boolean; /// True exporte de PS vers Sepi
  end;

{-----------------}
{ Global routines }
{-----------------}

{*
  Call-back pour l'événement OnUses du compilateur PascalScript
  @param Context    Contexte de compilation
  @param Sender     Compilateur Pascal Script
  @param UnitName   Nom de l'unité à charger
  @return True si l'unité a été chargée, False si elle n'a pas pu être trouvée
*}
function ScriptOnUses(Context: PCompilingContext; Sender: TPSPascalCompiler;
  const UnitName: string): Boolean;
var
  I: Integer;
  SepiUnit: TSepiUnit;
begin
  Result := True;

  try
    if AnsiSameText(UnitName, Context.SepiUnit.Name) then
      Result := False
    else if AnsiSameText(UnitName, SystemUnitName) then
    begin
      SepiUnit := Context.SepiUnit;
      for I := 0 to SepiUnit.UsedUnitCount-1 do
        SepiImportUnitInPSCompiler(SepiUnit.UsedUnits[I], Sender);
      SepiImportUnitInPSCompiler(SepiUnit, Sender);
    end else
    begin
      Context.SepiUnit.MoreUses([UnitName]);
      SepiUnit := Context.Root.FindMeta(UnitName) as TSepiUnit;
      SepiImportUnitInPSCompiler(SepiUnit, Sender);
    end;
  except
    on Error: ESepiUnitNotFoundError do
      Result := False;
  end;
end;

{*
  Vérifie que la déclaration Pascal Script correspond à la signature Sepi
  @param Root         Racine Sepi
  @param PSProc       Procédure Pascal Script
  @param SepiMethod   Méthode Sepi
  @return True si les déclarations correspondent, False sinon
*}
function CheckExport(Root: TSepiRoot; PSProc: TPSInternalProcedure;
  SepiMethod: TSepiMethod): Boolean;
const
  SepiKindToPSMode: array[TSepiParamKind] of TPSParameterMode = (
    pmIn, pmInOut, pmIn, pmOut
  );
var
  I: Integer;
  ParameterCount: Integer;
  SepiParam: TSepiParam;
  PSParam: TPSParameterDecl;
begin
  Result := False;

  with SepiMethod.Signature, PSProc.Decl do
  begin
    // Check calling convention
    if CallingConvention <> ccRegister then
      Exit;

    // Check parameter count
    // We must exclude the potential Result special parameter, because PS
    // doesn't treat it as Sepi does.
    //   (in register calling conv, Result will always be last)
    ParameterCount := ActualParamCount;
    if (ParameterCount > 0) and
      (ActualParams[ParameterCount-1].HiddenKind = hpResult) then
      Dec(ParameterCount);
    if ParameterCount <> ParamCount then
      Exit;

    // Check parameters
    for I := 0 to ParameterCount-1 do
    begin
      SepiParam := ActualParams[I];
      PSParam := Params[I];

      if PSParam.Mode <> SepiKindToPSMode[SepiParam.Kind] then
        Exit;
      if SepiParam.ParamType <> Root.GetType(PSParam.aType.Name) then
        Exit;
    end;

    // Check result type
    if ReturnType = nil then
    begin
      if Result <> nil then
        Exit;
    end else
    begin
      if ReturnType <> Root.GetType(Result.Name) then
        Exit;
    end;
  end;

  Result := True;
end;

{*
  Exporte une routine non trouvée dans Sepi
  @param SepiUnit   Unité Sepi contenante
  @param PSProc     Procédure Pascal Script
  @return True si la routine a été importée, False sinon
*}
function ExportUnexisting(SepiUnit: TSepiUnit;
  PSProc: TPSInternalProcedure): Boolean;
var
  Signature: string;
  I: Integer;
  PSParam: TPSParameterDecl;
  SepiType: TSepiType;
begin
  Signature := '';
  Result := False;

  with PSProc.Decl do
  begin
    // Parameters
    for I := 0 to ParamCount-1 do
    begin
      PSParam := Params[I];

      SepiType := SepiUnit.Root.GetType(PSParam.aType.Name);
      if SepiType = nil then
        Exit;

      if PSParam.Mode = pmOut then
        Signature := Signature + 'out '
      else if PSParam.Mode = pmInOut then
        Signature := Signature + 'var ';

      Signature := Signature + PSParam.OrgName + ': ' + SepiType.Name + '; ';
    end;

    // Put the brackets
    if Signature <> '' then
    begin
      SetLength(Signature, Length(Signature)-2);
      Signature := '('+Signature+')';
    end;

    // Return type
    if Result = nil then
      Signature := 'procedure' + Signature
    else
    begin
      SepiType := SepiUnit.Root.GetType(Result.Name);
      if SepiType = nil then
        Exit;

      Signature := 'function' + Signature + ': ' + SepiType.Name;
    end;

    // Add the method to the unit
    TSepiMethod.Create(SepiUnit, PSProc.OriginalName, nil, Signature);
  end;

  Result := True;
end;

{*
  Call-back pour l'événement OnExportCheck du compilateur Pascal Script
  @param Context    Contexte de compilation
  @param Sender     Compilateur Pascal Script
  @param Proc       Procédure Pascal Script
  @param ProcDecl   Déclaration de la procédure
  @return True si la déclaration est valide, False sinon
*}
function ScriptOnExportCheck(Context: PCompilingContext;
  Sender: TPSPascalCompiler; Proc: TPSInternalProcedure;
  const ProcDecl: string): Boolean;
var
  Meta: TSepiMeta;
  ClassName, MethodName: string;
begin
  if AnsiSameText(Proc.Name, '!MAIN') then
    Proc.OriginalName := '$MAIN';
  Result := False;

  with Context^ do
  begin
    // Search for a matching routine/method in Sepi
    Meta := SepiUnit.GetMeta(Proc.OriginalName);

    if (Meta = nil) and
      SplitToken(Proc.OriginalName, '_', ClassName, MethodName) then
    begin
      Meta := SepiUnit.GetMeta(ClassName);
      if Meta is TSepiClass then
      begin
        Meta := Meta.GetMeta(MethodName);
        Proc.OriginalName := ClassName + '.' + MethodName;
      end;
    end;

    // A match that isn't a method is a duplicate identifier
    if (Meta <> nil) and (not (Meta is TSepiMethod)) then
    begin
      Sender.MakeError(SepiUnit.Name, ecDuplicateIdentifier,
        Proc.OriginalName);
      Exit;
    end;

    // Check the export or export an unexisting
    if Meta <> nil then
    begin
      Result := CheckExport(Root, Proc, TSepiMethod(Meta));
      if not Result then
        Sender.MakeError(SepiUnit.Name, ecCustomError,
          sDeclDiffersFromPrevious);
    end else
    begin
      Result := True;
      if Context.ExportUnexistingProcs and
        ((not AnsiSameText(Proc.OriginalName, '$MAIN')) or
        (not Sender.IsUnit)) then
      begin
        if not ExportUnexisting(SepiUnit, Proc) then
          Sender.MakeWarning(SepiUnit.Name, ewCustomWarning,
            Format(sCantExportProc, [Proc.OriginalName]));
      end;
    end;
  end;
end;

{*
  Compile une unité Sepi-PS
  @param Source       Code source
  @param Output       Flux destination
  @param SepiUnit     Unité compilée
  @param Errors       Liste de chaînes où enregister les erreurs
  @return True si la compilation s'est bien déroulée, False en cas d'erreur
*}
function SepiPSCompile(const Source: string; Output: TStream;
  SepiUnit: TSepiUnit; Errors: TStrings;
  ExportUnexistingProcs: Boolean = True): Boolean;
var
  Root: TSepiRoot;
  Context: TCompilingContext;
  Method: TMethod;
  OnUses, OnExportCheck: Pointer;
  Compiler: TPSPascalCompiler;
  I: Integer;
  Compiled: string;
begin
  Compiled := '';

  Root := SepiUnit.Root;

  // Make compiling context
  Context.Root := Root;
  Context.SepiUnit := SepiUnit;
  Context.ExportUnexistingProcs := ExportUnexistingProcs;

  // Make procs for call-backs
  Method.Code := @ScriptOnUses;
  Method.Data := @Context;
  OnUses := MakeProcOfRegisterMethod(Method, 2);

  Method.Code := @ScriptOnExportCheck;
  OnExportCheck := MakeProcOfRegisterMethod(Method, 3);

  // Compile
  try
    Compiler := TPSPascalCompiler.Create;
    try
      @Compiler.OnUses := OnUses;
      @Compiler.OnExportCheck := OnExportCheck;
      Compiler.AllowNoBegin := True;
      Compiler.AllowUnit := True;
      try
        try
          Compiler.Compile(Source);
        except
          // work around a bug of PS (or is it my use of it that's wrong?)
          on Error: EInvalidPointer do;
        end;

        SepiUnit.Complete;
        Compiler.GetOutput(Compiled);
      finally
        for I := 0 to Compiler.MsgCount-1 do
          Errors.Add(Compiler.Msg[I].MessageToString);
      end;
    finally
      Compiler.Free;
    end;
  finally
    FreeProcOfMethod(OnExportCheck);
    FreeProcOfMethod(OnUses);
  end;

  Result := Compiled <> '';

  // Write the output
  if Result then
  begin
    SepiUnit.SaveToStream(Output);
    WriteStrToStream(Output, Compiled);
  end;
end;

{*
  Compile une unité Sepi-PS
  @param Source       Code source
  @param Output       Flux destination
  @param UnitName     Nom de l'unité
  @param Errors       Liste de chaînes où enregister les erreurs
  @param OnLoadUnit   Méthode de call-back pour le chargement d'une unité
  @return True si la compilation s'est bien déroulée, False en cas d'erreur
*}
function SepiPSCompile(const Source: string; Output: TStream;
  const UnitName: string; Errors: TStrings;
  const OnLoadUnit: TSepiLoadUnitEvent = nil): Boolean;
var
  Root: TSepiRoot;
  SepiUnit: TSepiUnit;
begin
  Root := TSepiRoot.Create;
  try
    if Assigned(OnLoadUnit) then
      Root.OnLoadUnit := OnLoadUnit
    else
      @Root.OnLoadUnit := @SepiPSLoadUnit;

    SepiUnit := TSepiUnit.Create(Root, UnitName, []);

    Result := SepiPSCompile(Source, Output, SepiUnit, Errors);
  finally
    Root.Free;
  end;
end;

end.

