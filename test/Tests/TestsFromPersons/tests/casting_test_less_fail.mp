program casting_test_less_fail;
VAR myBool : Boolean;
someNum : Integer;

BEGIN
  myBool := True;
  someNum := 7;
  writeln('This following code should fail');
writeln( myBool > someNum );
END
.