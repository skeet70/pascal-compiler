program casting_test_lessEqual;
  VAR intnumber : Integer;
  floatnumber: Float;

BEGIN
  
  intnumber := 500;
floatnumber := 500.50;
writeln('Program casting_test_less_equal');
  writeln('Output should be False');
  writeln( floatnumber <= intnumber);
  writeln('Output should be True');
  writeln( intnumber <= floatnumber );
  writeln('Output should be False');
  writeln( floatnumber <= 500.40 );
  writeln('Output should be True');
  writeln( floatnumber <= 500.50 );
  writeln('Output should be True');
writeln( intnumber <= 500 );
writeln('Testing to see if this is greater than or equal for 5 >= 6');
  writeln( 5 >= 6 );
  writeln('Testing to see if this is less than or equal for 5 <= 6');
  writeln(5 <= 6);
END
.