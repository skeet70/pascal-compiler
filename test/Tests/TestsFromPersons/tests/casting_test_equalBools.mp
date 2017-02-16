program casting_test_equalsBool;
VAR myBool : Boolean;

BEGIN
  myBool := True;
  writeln('Prgram casting_test_equalsBool');
  writeln('This next line should return True');
  writeln(myBool = True);
  writeln('This next line should return False');
  writeln(myBool = False);
  writeln('This next line should return False');
  writeln(False <> False);
  writeln('This next line should return True');
  writeln(myBool <> False);
END
.