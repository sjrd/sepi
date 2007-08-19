{*
  Ex�cuteur d'unit� Sepi-PS
  @author sjrd
  @version 1.0
*}
unit SepiPSExecuter;

interface

procedure SepiPSExecute(const UnitName : string);

implementation

uses
  SepiMetaUnits, SepiCompTypes, SepiPSLoader;

{*
  Ex�cute un script Sepi-PS
  @param UnitName   Nom de l'unit� � charger pour ex�cution
*}
procedure SepiPSExecute(const UnitName : string);
var Root : TSepiMetaRoot;
    Method : TMethod;
    SepiUnit : TSepiMetaUnit;
    SepiMainProc : TSepiMetaMethod;
    MainProc : procedure;
begin
  Root := TSepiMetaRoot.Create;
  try
    Method.Code := @SepiPSLoadUnit;
    Method.Data := nil;
    Root.OnLoadUnit := TSepiLoadUnitEvent(Method);

    SepiUnit := Root.LoadUnit(UnitName);
    SepiMainProc := SepiUnit.GetMeta('$MAIN') as TSepiMetaMethod;

    if SepiMainProc = nil then
      WriteLn('There is no main proc in this unit') else
    begin
      @MainProc := SepiMainProc.Code;
      MainProc;
    end;
  finally
    Root.Free;
  end;
end;

end.

