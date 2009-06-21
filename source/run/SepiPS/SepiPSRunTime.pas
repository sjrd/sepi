{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
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
-------------------------------------------------------------------------------}

{*
  Liaison Sepi-PS à l'exécution
  @author sjrd
  @version 1.0
*}
unit SepiPSRunTime;

interface

uses
  SepiReflectionCore, uPSRuntime;

procedure SepiRegisterClassesInPSExecuter(SepiRoot: TSepiRoot;
  PSExecuter: TPSExec);
procedure SepiRegisterProcsInPSExecuter(SepiUnit: TSepiUnit;
  PSExecuter: TPSExec);
procedure SepiRegisterVarsInPSExecuter(SepiUnit: TSepiUnit;
  PSExecuter: TPSExec);

function SepiLoadPSExecuter(SepiRoot: TSepiRoot;
  const SepiUnits: array of TSepiUnit; PSExecuter: TPSExec;
  const Compiled: string): Boolean;

implementation

uses
  SysUtils, TypInfo, StrUtils, ScUtils, ScStrUtils, ScDelphiLanguage,
  ScCompilerMagic, ScTypInfo, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, uPSUtils, SepiPSUtils;

type
  {*
    Classe privée TProtectedPSExec
    @author sjrd
    @version 1.0
  *}
  TProtectedPSExec = class(TPSExec)
  public
    function InnerfuseCall(_Self, Address: Pointer;
      CallingConv: TPSCallingConvention; Params: TPSList;
      ReturnValue: PPSVariantIFC): Boolean; reintroduce;
  end;

resourcestring
  RPS_CouldNotCallProc = 'Could not call proc';

{------------------------}
{ TProtectedPSExec class }
{------------------------}

{*
  Appelle une routine/méthode native dans un contexte Pascal Script
  @param _Self         Instance d'objet (nil pour une routine)
  @param Address       Pointeur sur le code de la routine/méthode à appeler
  @param CallingConv   Convention d'appel de la routine/méthode
  @param Params        Paramètres de l'appel
  @param ReturnValue   Emplacement où enregistrer la valeur de retour
  @return True si l'appel s'est déroulé sans exception, False sinon
*}
function TProtectedPSExec.InnerfuseCall(_Self, Address: Pointer;
  CallingConv: TPSCallingConvention; Params: TPSList;
  ReturnValue: PPSVariantIFC): Boolean;
begin
  Result := inherited InnerfuseCall(_Self, Address, CallingConv,
    Params, ReturnValue);
end;

{-----------------}
{ Global routines }
{-----------------}

{*
  Procédure de call-back pour un appel de méthode native
  @param Caller   Interpréteur Pascal Script
  @param Method   Descripteur de méthode
  @param Global   Environnement de variables globales
  @param Stack    Pile des paramètres
  @return True si l'appel est réussi, False sinon
*}
function ClassCallProcMethod(Caller: TPSExec; Method: TPSExternalProcRec;
  Global, Stack: TPSStack): Boolean;

  function NewSpecialParam(Data: Pointer): PPSVariantIFC;
  begin
    New(Result);
    Result.Dta := Data;
    Result.aType := Caller.FindType2(btU32);
    Result.VarParam := False;
  end;

var
  SepiMethod: TSepiMethod;
  Signature: TSepiSignature;
  TrueBoolValue: Integer;
  CallingConv: TPSCallingConvention;
  HasResult: Boolean;
  I, J: Integer;
  SelfParam, Param: PIFVariant;
  PSClassType: PIFTypeRec;
  Self: Pointer;
  ClassType: TClass;
  ParamList: TPSList;
  ResultValue: PPSVariantIFC;
  CurrStack: Cardinal;
  ProcPtr: Pointer;
  ExceptObject: TObject;
begin
  Result := False;
  SepiMethod := TSepiMethod(Method.Ext1);
  Signature := SepiMethod.Signature;
  TrueBoolValue := 1;

  // Extract calling convention and whether it has a return value or not
  CallingConv := CallingConvSepiToPS[Signature.CallingConvention];
  HasResult := Signature.Kind in [mkConstructor, mkFunction, mkClassFunction];

  // Get the Self parameter
  if HasResult then
    SelfParam := Stack[Stack.Count - 2]
  else
    SelfParam := Stack[Stack.Count - 1];

  // Get the Self value from the Self parameter
  if Signature.Kind = mkConstructor then
  begin
    // The Self parameter contains a reference to a Pascal Script class type
    if (SelfParam = nil) or (SelfParam.FType.BaseType <> btU32) then
      Exit;

    PSClassType := Caller.GetTypeNo(PPSVariantU32(SelfParam).Data);
    if PSClassType = nil then
      Exit;

    ClassType := (SepiMethod.Root.FindType(
      PSClassType.ExportName) as TSepiClass).DelphiClass;
    Self := ClassType;
  end else
  begin
    // The Self parameter contains the object reference
    if (SelfParam = nil) or (SelfParam.FType.BaseType <> btClass) then
      Exit;
    Self := PPSVariantClass(SelfParam).Data;
    ClassType := TObject(Self).ClassType;
  end;

  ParamList := TPSList.Create;
  try
    // Set up the parameter list
    CurrStack := Stack.Count - Signature.ParamCount - 2;
    if not HasResult then
      Inc(CurrStack);

    for I := 0 to Signature.ParamCount do // 0 for there is also the Self param
      ParamList.Add(nil);
    if Signature.Kind in [mkConstructor, mkDestructor] then
      ParamList.Add(nil);

    // Self parameter
    if CallingConv = cdPascal then
    begin
      ParamList[ParamList.Count-1] := NewSpecialParam(@Self);
      J := 1;
    end else
    begin
      ParamList[0] := NewSpecialParam(@Self);
      J := 0;
    end;

    // Special parameter of constructors and destructors
    if Signature.Kind in [mkConstructor, mkDestructor] then
    begin
      ParamList[1-J] := NewSpecialParam(@TrueBoolValue);
      Dec(J);
    end;

    // Normal parameters
    for I := Signature.ParamCount downto 1 do
    begin
      Param := Stack[CurrStack];
      ParamList[I-J] := NewPPSVariantIFC(Param, Signature.Params[I-1].ByRef);
      Inc(CurrStack);
    end;

    // Set up the return value
    if HasResult then
      ResultValue := NewPPSVariantIFC(Stack[CurrStack + 1], True)
    else
      ResultValue := nil;

    try
      // Find the method code
      case SepiMethod.LinkKind of
        mlkStatic: ProcPtr := SepiMethod.Code;
        mlkVirtual:
          ProcPtr := GetClassVirtualCode(ClassType, SepiMethod.VMTOffset);
        mlkDynamic, mlkMessage:
          ProcPtr := GetClassDynamicCode(ClassType, SepiMethod.DMTIndex);
      else
        Exit;
      end;

      try
        // Call the method
        Result := TProtectedPSExec(Caller).InnerfuseCall(
          nil, ProcPtr, CallingConv, ParamList, ResultValue);
      except
        // Pass the exception object to Pascal Script
        ExceptObject := AcquireExceptionObject;
        if ExceptObject = nil then
          Caller.CMD_Err(erCouldNotCallProc)
        else if ExceptObject is Exception then
          Caller.CMD_Err3(erCustomError, (ExceptObject as Exception).Message,
            ExceptObject)
        else
          Caller.CMD_Err3(erCustomError, RPS_CouldNotCallProc, ExceptObject);
        Result := False;
      end;
    finally
      DisposePPSVariantIFC(ResultValue);
    end;
  finally
    DisposePPSVariantIFCList(ParamList);
  end;
end;

{*
  Procédure de call-back pour un accès à un champ
  @param Caller   Interpréteur Pascal Script
  @param Method   Descripteur de méthode
  @param Global   Environnement de variables globales
  @param Stack    Pile des paramètres
  @return True si l'appel est réussi, False sinon
*}
function ClassCallProcField(Caller: TPSExec; Method: TPSExternalProcRec;
  Global, Stack: TPSStack): Boolean;
var
  SepiField: TSepiField;
  FieldType: TSepiType;
  IsWriteAccess: Boolean;
  SelfParam, ValueParam: PIFVariant;
  Value: PPSVariantIFC;
  SourceData, DestData: Pointer;
begin
  Result := False;
  SepiField := TSepiField(Method.Ext1);
  FieldType := SepiField.FieldType;
  IsWriteAccess := LongBool(Method.Ext2);
  Value := nil;

  try
    // Write access
    if IsWriteAccess then
    begin
      ValueParam := Stack[Stack.Count-2];
      Value := NewPPSVariantIFC(ValueParam, False);
      SourceData := Value.Dta;

      SelfParam := Stack[Stack.Count-1];
      if (SelfParam = nil) or (SelfParam.FType.BaseType <> btClass) then
        Exit;
      DestData := PPSVariantClass(SelfParam).Data;
      Inc(Integer(DestData), SepiField.Offset);
    end else

    // Read access
    begin
      SelfParam := Stack[Stack.Count-2];
      if (SelfParam = nil) or (SelfParam.FType.BaseType <> btClass) then
        Exit;
      SourceData := PPSVariantClass(SelfParam).Data;
      Inc(Integer(SourceData), SepiField.Offset);

      ValueParam := Stack[Stack.Count-1];
      Value := NewPPSVariantIFC(ValueParam, True);
      DestData := Value.Dta;
    end;

    // Copy data
    CopyData(SourceData^, DestData^, FieldType.Size, FieldType.TypeInfo);
  finally
    uPSRuntime.DisposePPSVariantIFC(Value);
  end;

  Result := True;
end;

{*
  Importe une méthode ou propriété d'une classe Sepi
  @param Sender   Interpréteur Pascal Script
  @param Method   Descripteur de méthode externe Pascal Script
  @param Tag      Tag de callback : racine Sepi
  @return True si la méthode a bien été trouvée et importée, False sinon
*}
function SepiSpecialProcImport(Sender: TPSExec; Method: TPSExternalProcRec;
  Tag: Pointer): Boolean;
var
  Root: TSepiRoot;
  Decl, ClassName, MethodName: string;
  IsWriteAccess: Boolean;
  ComponentClass, Component: TSepiComponent;
  Pos: Integer;
begin
  Result := False;
  Root := TSepiRoot(Tag);

  // Parse the declaration
  // 'class:' + ClassName + '|' + MethodName + '|' + Signature
  Decl := Method.Decl;
  Delete(Decl, 1, 6); // class:
  ClassName := ExtractFirstToken(Decl, '|');
  MethodName := ExtractFirstToken(Decl, '|');

  // Property write access method will have an '@' at the end of MethodName
  IsWriteAccess := (MethodName <> '') and
    (MethodName[Length(MethodName)] = '@');
  if IsWriteAccess then
    SetLength(MethodName, Length(MethodName)-1);

  // Find the class item
  ComponentClass := Root.GetType(ClassName);
  if ComponentClass = nil then
    Exit;
  Component := ComponentClass.GetComponent(MethodName);

  // Handle overloaded method
  if Component = nil then
  begin
    Pos := RightPos('_', MethodName);
    if Pos > 0 then
    begin
      MethodName[Pos] := '$';
      Component := ComponentClass.GetComponent(MethodName);
    end;

    if Component = nil then
      Exit;
  end;

  // Map properties to their accessor
  if Component is TSepiProperty then
  begin
    if IsWriteAccess then
      Component := TSepiProperty(Component).WriteAccess.Component
    else
      Component := TSepiProperty(Component).ReadAccess.Component;

    if Component = nil then
      Exit;
  end;

  // Replace an overloaded method by its first real method
  if Component is TSepiOverloadedMethod then
    Component := TSepiOverloadedMethod(Component).Methods[0];

  // Set the proper calling handler
  if Component is TSepiMethod then
  begin
    Method.ProcPtr := @ClassCallProcMethod;
    IsWriteAccess := False;
  end else
  begin
    Assert(Component is TSepiField);
    Method.ProcPtr := @ClassCallProcField;
  end;

  // Update method descriptor
  Method.Name := Component.Name;
  Method.Decl := Decl; // Signature
  Method.Ext1 := Component;
  Method.Ext2 := Pointer(IsWriteAccess);
  Result := True;
end;

{*
  Importe une routine Sepi dans un interpréteur Pascal Script
  @param Routine      Routine Sepi
  @param PSExecuter   Intepréteur Pascal Script
*}
procedure ImportRoutine(PSExecuter: TPSExec; Routine: TSepiMethod);
var
  CallingConv: TPSCallingConvention;
  PSName, SecondName: string;
begin
  with Routine do
  begin
    CallingConv := CallingConvSepiToPS[Signature.CallingConvention];
    if HandleOverloaded(Name, PSName, SecondName) then
      PSExecuter.RegisterDelphiFunction(Code, SecondName, CallingConv);
    PSExecuter.RegisterDelphiFunction(Code, PSName, CallingConv);
  end;
end;

{*
  Importe une variable globale Sepi dans un interpréteur Pascal Script
  @param Variable     Variable Sepi
  @param PSExecuter   Intepréteur Pascal Script
*}
procedure ImportVariable(PSExecuter: TPSExec; Variable: TSepiVariable);
var
  PSVar: PIFVariant;
  VariantVar: TPSVariantIFC;
begin
  PSVar := PSExecuter.GetVar2(Variable.Name);
  if PSVar = nil then // its type couldn't be imported
    Exit;

  VariantVar.Dta := @PPSVariantData(PSVar).Data;
  VariantVar.aType := PSVar.FType;
  VariantVar.VarParam := False;

  VNSetPointerTo(VariantVar, Variable.Value,
    PSExecuter.FindType2(btPointer));
end;

{*
  Importe le contenu d'une unité Sepi dans un interpréteur Pascal Script
  @param SepiUnit     Unité Sepi
  @param PSExecuter   Intepréteur Pascal Script
*}
procedure SepiImportUnitInPSExecuter(SepiUnit: TSepiUnit;
  PSExecuter: TPSExec);
var
  I: Integer;
  Child: TSepiComponent;
begin
  for I := 0 to SepiUnit.ChildCount-1 do
  begin
    Child := SepiUnit.Children[I];

    if Child is TSepiMethod then
      ImportRoutine(PSExecuter, TSepiMethod(Child))
    else if Child is TSepiVariable then
      ImportVariable(PSExecuter, TSepiVariable(Child));
  end;
end;

{*
  Recense les classes d'une racine Sepi dans un interpréteur Pascal Script
  @param SepiRoot     Racine Sepi
  @param PSExecuter   Interpréteur Pascal Script
*}
procedure SepiRegisterClassesInPSExecuter(SepiRoot: TSepiRoot;
  PSExecuter: TPSExec);
begin
  PSExecuter.AddSpecialProcImport('class',
    SepiSpecialProcImport, SepiRoot);
end;

{*
  Recense les routines d'une unité Sepi dans un interpréteur Pascal Script
  @param SepiUnit     Unité Sepi
  @param PSExecuter   Interpréteur Pascal Script
*}
procedure SepiRegisterProcsInPSExecuter(SepiUnit: TSepiUnit;
  PSExecuter: TPSExec);
var
  I: Integer;
  Child: TSepiComponent;
begin
  for I := 0 to SepiUnit.ChildCount-1 do
  begin
    Child := SepiUnit.Children[I];
    if Child is TSepiMethod then
      ImportRoutine(PSExecuter, TSepiMethod(Child));
  end;
end;

{*
  Recense les variables d'une unité Sepi dans un interpréteur Pascal Script
  @param SepiUnit     Unité Sepi
  @param PSExecuter   Interpréteur Pascal Script
*}
procedure SepiRegisterVarsInPSExecuter(SepiUnit: TSepiUnit;
  PSExecuter: TPSExec);
var
  I: Integer;
  Child: TSepiComponent;
begin
  for I := 0 to SepiUnit.ChildCount-1 do
  begin
    Child := SepiUnit.Children[I];
    if Child is TSepiVariable then
      ImportVariable(PSExecuter, TSepiVariable(Child));
  end;
end;

{*
  Charge complètement un script Pascal Script basé sur Sepi
  Cette routine est un raccourci pour les trois autres
  SepiRegisterClassesInPSExecuter, SepiRegisterProcsInPSExecuter et
  SepiRegisterVarsInPSExecuter, mais elle prend en charge elle-même le
  chargement de Pascal Script, ce qui empêche, ou du moins rend moins évident,
  l'ajout d'autres éléments.
  @param SepiRoot     Racine Sepi
  @param SepiUnits    Liste des unités Sepi à importer
  @param PSExecuter   Interpréteur Pascal Script
  @param Compiled     Données compilées Pascal Script
  @return True si le chargement s'est bien passé, False sinon
*}
function SepiLoadPSExecuter(SepiRoot: TSepiRoot;
  const SepiUnits: array of TSepiUnit; PSExecuter: TPSExec;
  const Compiled: string): Boolean;
var
  I: Integer;
begin
  SepiRegisterClassesInPSExecuter(SepiRoot, PSExecuter);
  for I := Low(SepiUnits) to High(SepiUnits) do
    SepiRegisterProcsInPSExecuter(SepiUnits[I], PSExecuter);

  Result := PSExecuter.LoadData(Compiled);
  if not Result then
    Exit;

  for I := Low(SepiUnits) to High(SepiUnits) do
    SepiRegisterVarsInPSExecuter(SepiUnits[I], PSExecuter);
end;

end.

