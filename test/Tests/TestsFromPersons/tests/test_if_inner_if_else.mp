program test_if_else_with_compound;
VAR int1: Integer;
flt1 : Float;

BEGIN
  int1 := 10;
  flt1 := 16.7;
  if( int1 = 10 )
    then
      BEGIN
      writeln('I am in outer IF');
      if( flt1 < int1 )
      then
	BEGIN
	  writeln('The float variable is smaller than the int variable. I am in inner IF.');
	END
      else
	  writeln('The float variable is larger than or equal to the int variable. I am in inner ELSE.');
	
      END
  else
    writeln('I am in outer ELSE. I should not be here.');

  writeln('I am out of the IF-ELSE.');
END.	