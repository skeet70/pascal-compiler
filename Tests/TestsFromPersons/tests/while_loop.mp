Program while_Loop;
VAR i : integer;
BEGIN
WRITELN('PROGRAM: while_Loop');
WRITELN('You should see jammin 5x, then hope jammin');

i := 5;

WHILE ( i > 0 ) do
BEGIN
  writeln('We''re jammin . . .');
  i := i - 1;
END
;    
writeln('And I hope you like jammin too.');

END
.
