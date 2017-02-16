{B level combination test
	Input:
	-5
	7
	15
	1, 7, 15,14,13,12,11,10,8,9,0,-1,-2,-3,-4,-5
}

program combinationTest;

var a, b, c, d : integer;

begin

read(b, c, d);

   for a:= 1 downto b do
   begin
	writeln(a);
	
	while (c < d) do
	begin
		writeln(c);
		c:= c+1;
		
		repeat 
			writeln(d);
			d := d-1;
		until (d  <  10)

	end;
   end;
	


end.

