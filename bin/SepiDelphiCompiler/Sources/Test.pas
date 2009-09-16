unit Test;

interface

uses
  SysUtils, Classes, ScDelphiLanguage;

type
  TPrintOnAddStrings = class(TStringList)
  public
    constructor Create;
    function Add(const Str: string): Integer; override;
  end;

implementation

constructor TPrintOnAddStrings.Create;
begin
  inherited Create;
end;

function TPrintOnAddStrings.Add(const Str: string): Integer;
begin
  inherited;
  WriteLn(Str);
end;

procedure Test(const Str: string; Strings: TStrings);
var
  Tab: array[0..1] of TVarRec;
  Line: string;
begin
  Tab[0].VAnsiString := Pointer(Str);
  Tab[0].VType := vtAnsiString;
  Tab[1].VInteger := Random(5);
  Tab[1].VType := vtInteger;
  Line := Format('%s %d', Tab);
  Strings.Add(Line);
end;

type
  TShorterInt = 0..15;

var
  IsFirstTitle: Boolean = True;

procedure WriteTitle(const Title: string);
begin
  if not IsFirstTitle then
    WriteLn('');

  WriteLn(Title);
  WriteLn(StringOfChar('-', 10));
  WriteLn('');

  IsFirstTitle := False;
end;

procedure TestSets;
const
  ShorterConst: TShorterInt = 5;
  EmptySet: TSysCharSet = [];
var
  C, D: Char;
  ShorterVar: TShorterInt;
begin
  WriteTitle('Test set construction and operations');

  ShorterVar := 10;

  C := '0';
  D := 'D';
  WriteLn(CharSetToStr(SwitchChars + [C, 'A'..D] - ['B', 'Q'] * ['B'..'E'] +
    [Chr(ShorterConst)..Chr(ShorterVar)] + EmptySet + (['Z']-[])));
end;

procedure TestChangeGlobalVar;
begin
  WriteTitle('Test changing a global variable');

  DecimalSeparator := ',';
  WriteLn(FloatToStr(3.1416));
  DecimalSeparator := '.';
  WriteLn(FloatToStr(2.1416));
end;

procedure TestIfCompilerDirective;
begin
  WriteTitle('Test {$IF} compiler directive');

  {$IF RTLVersion > 15}
    WriteLn('RTLVersion > 15');
  {$IFEND}

  {$IF 3+5 = 6}
    WriteLn('3+5 = 6');
  {$IFEND}

  {$IF Declared(TestSets) and not Defined(SOMETHING)}
    WriteLn('Cool! This beast is working!');
  {$IFEND}

  {$IF Defined(MSWINDOWS)}
    WriteLn('Compiled for Windows');
  {$IFEND}
end;

procedure TestMethodRef;
type
  TStringEvent = procedure(const Str: string) of object;
var
  Strings: TStrings;
  Add: TStringEvent;
  I: Integer;
begin
  WriteTitle('Test method references');

  Strings := TStringList.Create;
  try
    TMethod(Add).Code := nil;
    TMethod(Add).Data := Pointer(Strings);

    for I := 1 to 3 do
      if @Add <> nil then
        Add(IntToStr(I));

    for I := 0 to Strings.Count-1 do
      WriteLn(Strings[I]);
  finally
    Strings.Free;
  end;
end;

procedure TestIsAs;
var
  Strings: TStrings;
begin
  Strings := TStringList.Create;
  try
    if Strings is TStringList then
      WriteLn('Strings is TStringList')
    else
      WriteLn('Strings is not TStringList');

    (Strings as TStringList).CaseSensitive := True;

    if Strings is TList then
      WriteLn('Strings is TList')
    else
      WriteLn('Strings is not TList');

    try
      (Strings as TList).Add(nil);
    except
      on Error: EInvalidCast do
        WriteLn('Error when (Strings as TList): '+Error.Message);
    end;
  finally
    Strings.Free;
  end;
end;

procedure TestExceptionsAndClassDef;
var
  Strings: TStrings;
  I: Integer;
  Tab: array[0..10] of Integer;
begin
  WriteTitle('Test using a Sepi-defined class and raising exceptions');

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

        WriteLn(Strings[I]);

        if I < 5 then
          Break;
      end;

      I := Random(3);
      if I = 0 then
        raise EAbort.Create('Exception now!')
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

procedure Main;
begin
  Randomize;

  TestSets;
  TestChangeGlobalVar;
  TestIfCompilerDirective;
  TestMethodRef;
  TestIsAs;
  TestExceptionsAndClassDef;
end;

end.

