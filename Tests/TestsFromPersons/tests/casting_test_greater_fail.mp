program casting_test_greater_fail;
VAR myString : String;
myNum : Integer;

BEGIN
  myString := 'Just us sleeping chickens';
  myNum := 5;
  writeln('This code should fail');
  writeln( myString > myNum );
END
.