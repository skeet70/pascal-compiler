{
11
12
13
14
15
16
15
30
}

program test;

var x:integer; y:integer;

function f_one(a:integer):integer;
 begin
 a := a+1; 
 writeln(a);
 f_one := a;
 end;

function f_two(b:integer):integer;
 begin
 b := b+1;
 writeln(b);
 f_two := b;
 end;

function f_three(var c:integer):integer;
 begin
 c := c+1;
 writeln(c);
 f_three := c;
 end;

begin
  x := 10;
  y := 20;

  {
    these next three lines are illegal... f_three needs an 
    address (for its IN/OUT paramater), but cannot get one
    from the function returns
  }
  y := f_three(f_two(x));
  y := f_three(f_two(f_one(x)));
  x := f_three(f_one(f_two(y)));

  writeln(y+1);
  writeln(x+y);

end.
