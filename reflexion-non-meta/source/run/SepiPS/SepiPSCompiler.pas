{*
  Compilateur d'unit� Sepi-PS
  @author sjrd
  @version 1.0
*}
unit SepiPSCompiler;

interface

uses
  Classes, SepiMetaUnits;

function SepiPSCompile(const Source : string; Output : TStream;
  SepiUnit : TSepiMetaUnit; Errors : TStrings;
  ExportUnexistingProcs : boolean = True) : boolean; overload;
function SepiPSCompile(const Source : string; Output : TStream;
  const UnitName : string; Errors : TStrings;
  const OnLoadUnit : TSepiLoadUnitEvent = nil) : boolean; overload;

resourcestring
  sDeclDiffersFromPrevious = 'La d�claration diff�re de la pr�c�dente';
  sCantExportProc = 'Impossible d''exporter la routine %s';

implementation

uses
  Windows, SysUtils, TypInfo, ScUtils, ScStrUtils, ScDelphiLanguage, uPSUtils,
  uPSCompiler, SepiCompTypes, SepiPSCompileTime, SepiPSLoader;

type
  /// Pointeur vers TCompilingContext
  PCompilingContext = ^TCompilingContext;

  {*
    Contexte de compilation
    @author sjrd
    @version 1.0
  *}
  TCompilingContext = record
    Root : TSepiMetaRoot;            /// Racine Sepi
    SepiUnit : TSepiMetaUnit;        /// Unit� Sepi � compiler
    UsesList : array of string;      /// Liste des uses explicites
    ExportUnexistingProcs : boolean; /// True exporte de PS vers Sepi
  end;

{-----------------}
{ Global routines }
{-----------------}

{*
  Call-back pour l'�v�nement OnUses du compilateur PascalScript
  @param Context    Contexte de compilation
  @param Sender     Compilateur Pascal Script
  @param UnitName   Nom de l'unit� � charger
  @return True si l'unit� a �t� charg�e, False si elle n'a pas pu �tre trouv�e
*}
function ScriptOnUses(Context : PCompilingContext; Sender : TPSPascalCompiler;
  const UnitName : string) : boolean;
var I : integer;
    SepiUnit : TSepiMetaUnit;
begin
  try
    if AnsiSameText(UnitName, SystemUnitName) then
    begin
      SepiUnit := Context.SepiUnit;
      for I := 0 to SepiUnit.UsedUnitCount-1 do
        SepiImportUnitInPSCompiler(SepiUnit.UsedUnits[I], Sender);
      SepiImportUnitInPSCompiler(SepiUnit, Sender);
    end else
    begin
      SepiUnit := Context.Root.LoadUnit(UnitName);
      SepiImportUnitInPSCompiler(SepiUnit, Sender);

      SetLength(Context.UsesList, Length(Context.UsesList)+1);
      Context.UsesList[Length(Context.UsesList)-1] := UnitName;
    end;

    Result := True;
  except
    on Error : ESepiUnitNotFoundError do
      Result := False;
  end;
end;

{*
  V�rifie que la d�claration Pascal Script correspond � la signature Sepi
  @param Root         Racine Sepi
  @param PSProc       Proc�dure Pascal Script
  @param SepiMethod   M�thode Sepi
  @return True si les d�clarations correspondent, False sinon
*}
function CheckExport(Root : TSepiMetaRoot; PSProc : TPSInternalProcedure;
  SepiMethod : TSepiMetaMethod) : boolean;
var I : integer;
    ParameterCount : integer;
    SepiParam : TSepiMetaParam;
    PSParam : TPSParameterDecl;
begin
  Result := False;

  with SepiMethod.Signature, PSProc.Decl do
  begin
    // Check calling convention
    if CallingConvention <> ccRegister then
      exit;

    // Check parameter count
    //   (in register calling conv, Result will always be last)
    ParameterCount := ActualParamCount;
    if ReturnType <> nil then
      dec(ParameterCount);
    if ParameterCount <> ParamCount then
      exit;

    // Check parameters
    for I := 0 to ParameterCount-1 do
    begin
      SepiParam := ActualParams[I];
      PSParam := Params[I];

      case PSParam.Mode of
        pmIn :
          if (pfVar in SepiParam.Flags) or
             (pfOut in SepiParam.Flags) then exit;
        pmOut : if not (pfOut in SepiParam.Flags) then exit;
        pmInOut : if not (pfVar in SepiParam.Flags) then exit;
      end;

      if SepiParam.ParamType <> Root.GetType(PSParam.aType.Name) then
        exit;
    end;

    // Check result type
    if ReturnType = nil then
    begin
      if Result <> nil then
        exit;
    end else
    begin
      if ReturnType <> Root.GetType(Result.Name) then
        exit;
    end;
  end;

  Result := True;
end;

{*
  Exporte une routine non trouv�e dans Sepi
  @param SepiUnit   Unit� Sepi contenante
  @param PSProc     Proc�dure Pascal Script
  @return True si la routine a �t� import�e, False sinon
*}
function ExportUnexisting(SepiUnit : TSepiMetaUnit;
  PSProc : TPSInternalProcedure) : boolean;
var Signature : string;
    I : integer;
    PSParam : TPSParameterDecl;
    SepiType : TSepiType;
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
      if SepiType = nil then exit;

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
      Signature := 'procedure' + Signature else
    begin
      SepiType := SepiUnit.Root.GetType(Result.Name);
      if SepiType = nil then exit;

      Signature := 'function' + Signature + ': ' + SepiType.Name;
    end;

    // Add the method to the unit
    TSepiMetaMethod.Create(SepiUnit, PSProc.OriginalName, nil, Signature);
  end;

  Result := True;
end;

{*
  Call-back pour l'�v�nement OnExportCheck du compilateur Pascal Script
  @param Context    Contexte de compilation
  @param Sender     Compilateur Pascal Script
  @param Proc       Proc�dure Pascal Script
  @param ProcDecl   D�claration de la proc�dure
  @return True si la d�claration est valide, False sinon
*}
function ScriptOnExportCheck(Context : PCompilingContext;
  Sender : TPSPascalCompiler; Proc : TPSInternalProcedure;
  const ProcDecl : string) : boolean;
var Meta : TSepiMeta;
    ClassName, MethodName : string;
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
    if (Meta <> nil) and (not (Meta is TSepiMetaMethod)) then
    begin
      Sender.MakeError(SepiUnit.Name, ecDuplicateIdentifier, Proc.OriginalName);
      exit;
    end;

    // Check the export or export an unexisting
    if Meta <> nil then
    begin
      Result := CheckExport(Root, Proc, TSepiMetaMethod(Meta));
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
          Sender.MakeWarning(Proc.DeclareUnit, ewCustomWarning,
            Format(sCantExportProc, [Proc.OriginalName]));
      end;
    end;
  end;
end;

{*
  Compile une unit� Sepi-PS
  @param Source       Code source
  @param Output       Flux destination
  @param SepiUnit     Unit� compil�e
  @param Errors       Liste de cha�nes o� enregister les erreurs
  @return True si la compilation s'est bien d�roul�e, False en cas d'erreur
*}
function SepiPSCompile(const Source : string; Output : TStream;
  SepiUnit : TSepiMetaUnit; Errors : TStrings;
  ExportUnexistingProcs : boolean = True) : boolean;
var Root : TSepiMetaRoot;
    Context : TCompilingContext;
    Method : TMethod;
    OnUses, OnExportCheck : Pointer;
    Compiler: TPSPascalCompiler;
    I : integer;
    Compiled : string;
begin
  Compiled := '';

  Root := SepiUnit.Root;

  // Make compiling context
  Context.Root := Root;
  Context.SepiUnit := SepiUnit;
  SetLength(Context.UsesList, 1);
  Context.UsesList[0] := SystemUnitName;
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
          on Error : EInvalidPointer do;
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
    SepiUnit.MoreUses(Context.UsesList);
    SepiUnit.SaveToStream(Output);

    I := Length(Context.UsesList);
    Output.WriteBuffer(I, 4);
    for I := 0 to Length(Context.UsesList)-1 do
      WriteStrToStream(Output, Context.UsesList[I]);

    WriteStrToStream(Output, Compiled);
  end;
end;

{*
  Compile une unit� Sepi-PS
  @param Source       Code source
  @param Output       Flux destination
  @param UnitName     Nom de l'unit�
  @param Errors       Liste de cha�nes o� enregister les erreurs
  @param OnLoadUnit   M�thode de call-back pour le chargement d'une unit�
  @return True si la compilation s'est bien d�roul�e, False en cas d'erreur
*}
function SepiPSCompile(const Source : string; Output : TStream;
  const UnitName : string; Errors : TStrings;
  const OnLoadUnit : TSepiLoadUnitEvent = nil) : boolean;
var Root : TSepiMetaRoot;
    SepiUnit : TSepiMetaUnit;
begin
  Root := TSepiMetaRoot.Create;
  try
    if Assigned(OnLoadUnit) then
      Root.OnLoadUnit := OnLoadUnit
    else
      @Root.OnLoadUnit := @SepiPSLoadUnit;

    SepiUnit := TSepiMetaUnit.Create(Root, UnitName, []);

    Result := SepiPSCompile(Source, Output, SepiUnit, Errors);
  finally
    Root.Free;
  end;
end;

end.

