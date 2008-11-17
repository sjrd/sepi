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

    try
      for I := Strings.Count-1 downto 0 do
      begin
        if I = 7 then
          Continue;

        WriteLn(Strings.Strings[I]);

        if I < 5 then
          Break;
      end;

      if Random(2) > 0 then
        raise Exception.Create('Exception now!')
      else
        Exit;

      WriteLn('Will not appear');
    except
      WriteLn('Will appear if there was an exception');
      raise;
    end;
  finally
    Strings.Free;
    WriteLn('Will appear anyway');
  end;
  
  WriteLn('Again, will not appear');
end;

end.

