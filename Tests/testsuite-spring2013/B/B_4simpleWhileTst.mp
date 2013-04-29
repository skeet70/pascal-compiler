{While test
	Input:
	20
	10

		
	Output:
	20
	19
	18
	17
	16
	15
	14
	13
	12
	11
	10
	
	20
	19
	18
	17
	16
	15
	14
	13
	12
	11
	10
	9

	0
	1
	2
	3
	4
	5
	6
	7
	8
	9
	10

	0
	1
	2
	3
	4
	5
	6
	7
	8
	9
	10
	11
	}
program whileTest;

var a, b: integer;

begin

read(a, b);

	writeln(a);

	while a > b do
	begin
	  a := a - 1;
	  writeln(a);
	end;

	a := 20;
	writeln(a);

	while a >= b do
	begin
	  a := a - 1;
	  writeln(a);
	end;

	a:= 0;
	writeln(a);

	while a < b do
	begin
	  a := a + 1;
	  writeln(a);
	end;

	a := 0;
	writeln(a);

	while a <= b do
	begin
	  a := a + 1;
	  writeln(a);
	end;
	
	
end.
