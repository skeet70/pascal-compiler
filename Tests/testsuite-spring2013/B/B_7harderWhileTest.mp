program harderWhileTest;
          
var a, b, c, d, e, f, g, h, i, j, k, l, m: integer;
          
begin

          
{SINGLE COMPARISON}
	writeln(1111111);
	a := 0;
	{ exp OR exp }
        while ( (5 * 2) > a) or ( 4 > a ) do
        begin
          writeln(a);
          a := a + 1;
        end;

	writeln(2222222);
	a := 0;
	{ exp OR exp }
        while ( (5 * 2) < a) or ( 4 > a ) do
        begin
          writeln(a);
          a := a + 1;
        end;

	writeln(3333333);
	a := 0;
	{ exp OR exp }
        while ( (5 * 2) > a) or ( 400 < a ) do
        begin
          writeln(a);
          a := a + 1;
        end;

	writeln(4444444);
	a := 0;
	{ exp OR exp }
        while ( (5 * 2) < a) or ( 400 < a ) do
        begin
          writeln(a);
          a := a + 1;
        end;



	writeln(1111111);
	a := 0;
	{ exp AND exp }  
        while (a <= 10) and (a <= 1) do
        begin	            
	  writeln(a);
          a := a + 1;
        end;

	writeln(2222222);
	a := 0;
	{ exp AND exp }  
        while (a <= 10) and (a =  1) do
        begin	            
	  writeln(a);
          a := a + 1;
        end;

	writeln(3333333);
	a := 0;
	{ exp AND exp }  
        while (a <> (0*0)) and (a < 1) do
        begin	            
	  writeln(a);
          a := a + 1;
        end;

	writeln(4444444);
	a := 0;
	{ exp AND exp }  
        while (a <> (0*0)) and (a > 1) do
        begin	            
	  writeln(a);
          a := a + 1;
        end;

{DOUBLE COMPARISON}

	read(a, b, c, d);

	{ exp AND exp AND exp }
	writeln(2222222);
	a := 0;
        while (a < 3) and (2 < d) and (b < 10) do
        begin
          a := a + 1;
          writeln(a);
        end;

	{ exp OR exp AND exp }
	writeln(3333333);
	a := 0;
        while (a < 3) or (2 > d) and (b < 10) do
        begin
          a := a + 1;
          writeln(a);
        end;

	{ exp AND exp OR exp }
	writeln(4444444);
	a := 0;
        while (a < 3) and (2 < d) or (b > 10) do
        begin
          a := a + 1;
          writeln(a);
        end;






	 

end.
