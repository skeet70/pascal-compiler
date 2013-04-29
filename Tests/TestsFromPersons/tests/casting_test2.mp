program CASTING_TEST_2;
VAR int_num : Integer;
float_num : Float;

BEGIN
  int_num := 4;
  float_num := 4.1;
  writeln('This should return True');
  writeln( float_num > int_num);
  writeln('This should return False');
  writeln( int_num > float_num);
END
.
