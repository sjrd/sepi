unit Test;

interface

uses
  SysUtils, Classes, ScDelphiLanguage;

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

type
  TShorterInt = 0..15;

procedure Main;
const
  ShorterConst: TShorterInt = 5;
  EmptySet: TSysCharSet = [];
var
  Strings: TStrings;
  I: Integer;
  Tab: array[0..10] of Integer;
  C, D: Char;
  ShorterVar: TShorterInt;
begin
  Randomize;

  ShorterVar := 10;

  C := '0';
  D := 'D';
  WriteLn(CharSetToStr(SwitchChars + [C, 'A'..D] - ['B', 'Q'] * ['B'..'E'] +
    [Chr(ShorterConst)..Chr(ShorterVar)] + EmptySet + (['Z']-[])));

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

      I := Random(3);
      if I = 0 then
        raise Exception.Create('Exception now!')
      else if I = 1 then
        StrToInt('I am not an Integer!')
      else
        Exit;

      WriteLn('Will not appear');
    except
      on Error: EConvertError do
        WriteLn('Hey, I catched a EConvertError: ' + Error.Message);
      {else
        WriteLn('Unknown exception: I reraise it');
        raise;}
    end;
  finally
    Strings.Free;
    WriteLn('Will appear anyway');
  end;
  
  WriteLn('Will appear only if that was a EConvertError, which was catched');
end;

end.

