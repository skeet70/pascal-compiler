program casting_test_equals;
VAR float1 : Float;
float2 : Float;
int1 : Integer;
int2 : Integer;

BEGIN
  float1 := 7.15;
  float2 := 88.88;
  int1 := 505;
int2 := 1;
writeln('Program casting_test_equals');
  writeln('Output should be False');
  writeln(float1 = int1);
  writeln('Output should be False');
  writeln(int2 = float1);
  writeln('Output should be True');
  writeln(int1 = 505.00);
  writeln('Output should be True');
  writeln(float1 = 7.150);
  writeln('Output should be False');
  writeln(int2 = 2);
END
.