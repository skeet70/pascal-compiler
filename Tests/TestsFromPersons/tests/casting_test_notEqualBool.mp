program casting_test_notEqualBool;

BEGIN
  writeln('Next line should be False');
  writeln(True <> True);
  writeln('Next line should be True');
  writeln(True <> False);
  writeln('Next line should be True');
  writeln( (True <> False) <> False);
END
.