{*
  Utilitaires pour les liaisons Sepi-PS
  @author sjrd
  @version 1.0
*}
unit SepiPSUtils;

interface

uses
  SepiCompTypes, uPSUtils;

function HandleOverloaded(const Name : string;
  out PSName, SecondName : string) : boolean;

const
  /// Tableau de conversion des conventions d'appel Sepi vers Pascal Script
  CallingConvSepiToPS : array[TCallingConvention] of TPSCallingConvention = (
    cdRegister, cdCdecl, cdPascal, cdStdCall, cdSafecall
  );

implementation

uses
  Windows, SysUtils, StrUtils, ScStrUtils;

{*
  Gère le cas des noms de méthodes surchargées
  @param Name         Nom Sepi de la méthode
  @param PSName       Nom Pascal Script de la méthode
  @param SecondName   Variante Pascal Script (seulement si renvoie True)
  @return True si SecondName doit être utilisé, False sinon
*}
function HandleOverloaded(const Name : string;
  out PSName, SecondName : string) : boolean;
var StrOverloadIndex : string;
begin
  if AnsiStartsText('OL$', Name) then
  begin
    PSName := Copy(Name, 4, MaxInt);
    SplitToken(PSName, '$', SecondName, StrOverloadIndex);
    PSName[Pos('$', PSName)] := '_';
    Result := StrToInt(StrOverloadIndex) = 0;
  end else
  begin
    PSName := Name;
    Result := False;
  end;
end;

end.

