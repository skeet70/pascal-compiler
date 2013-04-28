program casting_test_equals_fail;

BEGIN
  writeln('Next line should break');
writeln( 'This String' = 'That String');
END
.