program test_if_with_compound;
VAR num1: Integer;
num2: Integer;
float1,float2 : Float;

BEGIN
  num1 := 0;
  num2 := 5;
  if( ( (4 * 5) >= ( 100 mod 19) ) and ( (3 + 3 + 3 * 3) < (12 * 45 / 2) ) )
    then
      BEGIN
      writeln('All up in the then portion');
      for num1 := (1 * 5 div 5) to num2
      do writeln(num1);
      writeln('Outa my for loops');
        BEGIN
	  writeln('I started another statement!');
	END
      END
      else
	writeln('How did we get here?');
  writeln('All done with the program!');
  END.	