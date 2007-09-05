{*
  Chargeur d'unité Sepi-PS
  @author sjrd
  @version 1.0
*}
unit SepiPSLoader;

interface

uses
  SysUtils, Classes, SepiReflectionCore;

function LoadSepiPSUnitFromStream(Root : TSepiMetaRoot;
  Stream : TStream) : TSepiMetaUnit;
function LoadSepiPSUnitFromFile(Root : TSepiMetaRoot;
  const FileName : TFileName) : TSepiMetaUnit;
function LoadSepiPSUnitFromName(Root : TSepiMetaRoot;
  const UnitName : string) : TSepiMetaUnit;

function SepiPSLoadUnit(Self : TObject; Root : TSepiMetaRoot;
  const UnitName : string) : TSepiMetaUnit;

const {don't localize}
  /// Extension des fichiers unité Sepi-PS
  sSepiPSExtension = '.sps';

implementation

uses
  Windows, ScUtils, ScDelphiLanguage, SepiMembers, uPSRunTime, SepiPSRuntime;

{-----------------}
{ Global routines }
{-----------------}

{*
  Exécute la main proc d'un interpréteur Pascal Script
  @param PSExecuter   Interpréteur Pascal Script
*}
procedure MainProc(PSExecuter : TPSExec);
begin
  if not PSExecuter.RunScript then
    PSExecuter.RaiseCurrentException;
end;

{*
  Lie une routine de l'interpréteur Pascal Script à son correspondant Sepi
  @param Routine      Routine Sepi
  @param PSExecuter   Interpréteur Pascal Script
*}
procedure LinkPSToSepiRoutine(Routine : TSepiMetaMethod; PSExecuter : TPSExec);
var ProcName : string;
    Method : TMethod;
begin
  if AnsiSameText(Routine.Name, '$MAIN') then
  begin
    // Main proc of the Pascal Script executer
    Method.Code := @MainProc;
    Method.Data := PSExecuter;
  end else
  begin
    // Normal routine or method
    ProcName := Routine.Name;
    if Routine.Owner is TSepiClass then
      ProcName := Routine.Owner.Name + '_' + ProcName;
    Method := PSExecuter.GetProcAsMethodN(ProcName);
  end;

  Routine.SetCodeMethod(Method);
end;

{*
  Lie chaque élément de l'interpréteur Pascal Script à son correspondant Sepi
  @param SepiUnit     Unité Sepi
  @param PSExecuter   Interpréteur Pascal Script
*}
procedure LinkPSToSepiClass(SepiClass : TSepiClass; PSExecuter : TPSExec);
var I : integer;
    Child : TSepiMeta;
begin
  for I := 0 to SepiClass.ChildCount-1 do
  begin
    Child := SepiClass.Children[I];

    if Child is TSepiMetaMethod then
      LinkPSToSepiRoutine(TSepiMetaMethod(Child), PSExecuter);
  end;
end;

{*
  Lie chaque élément de l'interpréteur Pascal Script à son correspondant Sepi
  @param SepiUnit     Unité Sepi
  @param PSExecuter   Interpréteur Pascal Script
*}
procedure LinkPSToSepi(SepiUnit : TSepiMetaUnit; PSExecuter : TPSExec);
var I : integer;
    Child : TSepiMeta;
begin
  for I := 0 to SepiUnit.ChildCount-1 do
  begin
    Child := SepiUnit.Children[I];

    if Child is TSepiMetaMethod then
      LinkPSToSepiRoutine(TSepiMetaMethod(Child), PSExecuter)
    else if Child is TSepiClass then
      LinkPSToSepiClass(TSepiClass(Child), PSExecuter);
  end;
end;

{*
  Charge une unité Sepi-PS depuis un flux
  @param Root     Racine Sepi
  @param Stream   Flux source
  @return Unité Sepi chargée
*}
function LoadSepiPSUnitFromStream(Root : TSepiMetaRoot;
  Stream : TStream) : TSepiMetaUnit;
var I, Count : integer;
    UsesList : array of TSepiMetaUnit;
    PSExecuter : TPSExec;
begin
  // Load the Sepi unit
  Result := TSepiMetaUnit.LoadFromStream(Root, Stream);
  try
    // Load the uses list
    Stream.ReadBuffer(Count, 4);
    SetLength(UsesList, Count);
    for I := 0 to Count-1 do
      UsesList[I] := Root.FindMeta(ReadStrFromStream(Stream)) as TSepiMetaUnit;

    // Create the PS executer
    PSExecuter := TPSExec.Create;
    Result.AddObjResource(PSExecuter);

    // Load the PS unit
    SepiLoadPSExecuter(Root, UsesList, PSExecuter,
      ReadStrFromStream(Stream));

    // Link PS unit to Sepi unit
    LinkPSToSepi(Result, PSExecuter);
  except
    Result.Free;
    raise;
  end;
end;

{*
  Charge une unité Sepi-PS depuis un fichier
  @param Root       Racine Sepi
  @param FileName   Nom du fichier
  @return Unité Sepi chargée
*}
function LoadSepiPSUnitFromFile(Root : TSepiMetaRoot;
  const FileName : TFileName) : TSepiMetaUnit;
var Stream : TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadSepiPSUnitFromStream(Root, Stream);
  finally
    Stream.Free;
  end;
end;

{*
  Charge une unité Sepi-PS depuis son nom
  @param Root       Racine Sepi
  @param UnitName   Nom de l'unité
  @return Unité Sepi chargée
*}
function LoadSepiPSUnitFromName(Root : TSepiMetaRoot;
  const UnitName : string) : TSepiMetaUnit;
var FileName : TFileName;
begin
  FileName := UnitName + sSepiPSExtension;
  if FileExists(FileName) then
    Result := LoadSepiPSUnitFromFile(Root, FileName)
  else
    Result := nil;
end;

{*
  Routine de call-back pour l'événement OnLoadUnit de TSepiMetaRoot
  @param Self       Objet courant, non utilisé
  @param Root       Racine Sepi
  @param UnitName   Nom de l'unité
  @return Unité Sepi chargée
*}
function SepiPSLoadUnit(Self : TObject; Root : TSepiMetaRoot;
  const UnitName : string) : TSepiMetaUnit;
begin
  Result := LoadSepiPSUnitFromName(Root, UnitName);
end;

end.

