Program repeat_UNTIL;
VAR A : Integer;
BEGIN
writeln('Program: repeat_UNTIL');
writeln('If loop is right, you should see BONGO! 3 times');
A := 3;
REPEAT
  writeln('BONGO!');
  A:= A - 1;
UNTIL
  (A < 1)
; {end repeat block}
  
writeln('Done with REPEAT');

END
.
