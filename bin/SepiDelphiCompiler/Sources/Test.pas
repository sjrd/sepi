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
  Randomize;

  Strings := TPrintOnAddStrings.Create;
  try
    for I := 0 to 10 do
      Tab[I] := I*I;

    for I := 0 to 10 do
      Test(IntToStr(Tab[I]), Strings);

    for I := Strings.Count-1 downto 0 do
      WriteLn(Strings.Strings[I]);

    try
      raise Exception.Create('Exception now!');
    
      WriteLn('Will not appear');
    except
      WriteLn('Will appear because there was an exception');
      raise;
    end;
  finally
    Strings.Free;
    WriteLn('Will appear anyway');
  end;
end;

end.

