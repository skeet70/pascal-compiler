program casting_test_less;
VAR floatnumber : Float;
intnumber : Integer;
floatnumber2 : Float;
intnumber2 : Integer;

BEGIN
  floatnumber := 3.14;
  floatnumber2 := -5.5;
  intnumber := 13;
intnumber2 := 7;
  writeln('Program casting_test_less');
  writeln('Output should be False for 3.14 < -5.5');
  writeln( floatnumber < floatnumber2 );
  writeln('Output should be True');
  writeln( floatnumber < intnumber );
  writeln('Output should be False');
  writeln( intnumber2 < floatnumber2 );
  writeln('Ouput should be True');
  writeln( intnumber2 < intnumber );
  writeln('Output should be False for hardcoded float < int');
  writeln( 105.501 < 100 );
  writeln('Output should be True for hardcoded int < float');
  writeln( 50 < 50.001 );
END
.