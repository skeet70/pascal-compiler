program casting_test_notEqual;
VAR float1: Float;
int1 : Integer;

BEGIN
  float1 := 65.666;
  int1 := 50;
  writeln('This next line should return True');
  writeln(float1 <> int1);
  writeln('This next line should return False');
  writeln(float1 <> 65.666);
  writeln('This next line should return True');
  writeln(int1 <> 50.05);
  writeln('This next line should return False');
  writeln(int1 <> 50);
END
.