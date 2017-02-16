{
input:25
26
27
28
26
}

program test;

var x:integer; y:integer;

function f_one(a:integer):integer;
 begin
 a := a+1;
 writeln(a);
 f_one := a;
 end;

function f_two(var c:integer):integer;
 begin
 read(c);
 c := c+1;
 writeln(c);
 f_two := c;
 end;

begin
  y := 20;

  y := f_two(x);
  y := f_one(y);

  writeln(y+1);
  writeln(-12+x+12);

end.
