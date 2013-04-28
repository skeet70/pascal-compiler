Program Expression_mod_test;
BEGIN
writeln('Expression_mod_test');
writeln('expected result: 1');
writeln(7 mod 3);
writeln('expected result: 0');
writeln(12 mod 3);
writeln('probably will output 0');
writeln( 30.1 mod 10 );
writeln('probably will output 2');
writeln(26 mod 6.9);
writeln('expected result: 1');
writeln(7.3 mod 3.8);
END
.
