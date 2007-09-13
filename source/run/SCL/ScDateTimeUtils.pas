{*
  Définit quelques routines de dates et heures
  @author sjrd
  @version 1.0
*}
unit ScDateTimeUtils;

interface

function GetMonthName(Month: Integer; Maj: Boolean = True): string;

implementation

uses
  ScConsts;

{*
  Renvoie le nom d'un mois
  @param Month   Mois basé sur 1
  @param Maj     Indique si la première lettre doit être une majuscule
  @return Le nom du mois Month, avec une majuscule si Maj vaut True
*}
function GetMonthName(Month: Integer; Maj: Boolean = True): string;
begin
  case Month of
    1: Result := sJanuary;
    2: Result := sFebruary;
    3: Result := sMarch;
    4: Result := sApril;
    5: Result := sMay;
    6: Result := sJune;
    7: Result := sJuly;
    8: Result := sAugust;
    9: Result := sSeptember;
    10: Result := sOctober;
    11: Result := sNovember;
    12: Result := sDecember;
  else
    Result := '';
  end;
  if Maj and (Result <> '') then
    Dec(Result[1], 32);
end;

end.

