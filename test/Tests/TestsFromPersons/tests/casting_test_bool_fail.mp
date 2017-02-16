program casting_test_bool_fail;

BEGIN
  writeln('This next line should fail');
writeln( True > False );
END
.