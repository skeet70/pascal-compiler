program casting_test_greaterEqual_fail;
VAR myString : String;
myOtherString : String;

BEGIN
  myString := 'Mudkipz';
  myOtherString := 'I heard u leik em';
  writeln('This next line should fail');
writeln( myString <= myOtherString );
END
.