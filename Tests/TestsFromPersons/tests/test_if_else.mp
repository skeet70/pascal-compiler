Program IF_else;
VAR A : Integer;
BEGIN
A := 2;
if( A > 3 )
then
BEGIN
  writeln('A is greater than 3');
END
ELSE
BEGIN
  writeln('A is not greater than 3');
END
; {end if block}
    
writeln('Done with if');

END
.
