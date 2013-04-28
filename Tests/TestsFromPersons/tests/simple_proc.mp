Program simple_PROC;

Procedure do_it;
    BEGIN
        writeln('I should come second in order of execution');
    END;

BEGIN
writeln('Simple procedure test - no arguments');

writeln('I should first in order of execution');
do_it;

END
.
