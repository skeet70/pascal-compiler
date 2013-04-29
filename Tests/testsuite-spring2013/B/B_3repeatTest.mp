{repeat test}
program repeatTest;

var a, b: integer;

begin

repeat
 read(a);
 read(b);
 writeln(a, b);
until a > b;

end.

