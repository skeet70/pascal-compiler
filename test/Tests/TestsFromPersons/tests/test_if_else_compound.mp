program test_if_else_with_compound;
VAR int1: Integer;

BEGIN
  int1 := 10;
  if( int1 = 0 )
    then
      BEGIN
      writeln('I am in IF');
      END
  else
    BEGIN
     if( int1 = ((((5 * 3) - 3) * 2) - 14) ) then
     BEGIN
       writeln('I am in ELSE. Compound statement in ELSE works.');
      END;
   END
     ;
     
   
  writeln('I am out of the IF-ELSE.');
END.	