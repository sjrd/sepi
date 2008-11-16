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

procedure Test(const Str: string; Strings: TStrings);
var
  Line: string;
begin
  Line := Str + ' ' + IntToStr(Random(5));
  Strings.Add(Line);
end;

procedure Main;
var
  Strings: TStrings;
  I: Integer;
  Tab: array[0..10] of Integer;
begin
  Strings := TPrintOnAddStrings.Create;

  for I := 0 to 10 do
    Tab[I] := I*I;

  for I := 0 to 10 do
    Test(IntToStr(Tab[I]), Strings);
    
  for I := Strings.Count-1 downto 0 do
    WriteLn(Strings.Strings[I]);
    
  Strings.Free;
end;

end.

