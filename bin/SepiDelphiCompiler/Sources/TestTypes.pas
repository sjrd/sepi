unit Test;

interface

uses
  Types, SysUtils;

implementation

procedure Test(const Str: string);
var
  SomeLocal: Integer;
begin
  WriteLn(Str + ' ' + IntToStr(Random(10)));
end;

procedure Main;
var
  P: TPoint;
  I: Integer;
begin
  Randomize;
  P := Point(5, 7);
  I := P.X + P.Y;
  WriteLn('Point is (' + IntToStr(P.X) + ', ' + IntToStr(P.Y) + ')');
  WriteLn('Sum is ' + IntToStr(I));
  Test('Hello');
  Test('World!');
end;

end.

