unit Test;

interface

uses
  SysUtils, Classes;

type
  TPrintOnAddStrings = class(TStringList)
  public
    function Add(const Str: string): Integer; override;
  end;

implementation

function TPrintOnAddStrings.Add(const Str: string): Integer;
begin
  Result := inherited Add(Str);
  WriteLn(Str);
end;

procedure Test(const Str: string);
var
  SomeLocal: Integer;
begin
  SomeLocal := Random(5);
  WriteLn(Str + ' ' + IntToStr(SomeLocal));
end;

procedure Main;
var
  I: Integer;
  Tab: array[0..10] of Integer;
begin
  for I := 0 to 10 do
    Tab[I] := I*I;

  for I := 0 to 10 do
    Test(IntToStr(Tab[I]));
end;

end.

