{
   input: 9 8 7 6
   output:
      13 -1 14 3.5 1.166...
      16 -1 18 4 4.5
}

program writelnTest;
var a, b : integer;
    c, d : float;

begin
   read(a,b);
   read(c,d);

   {math with two floats}
   writeln(c + d);
   writeln(d - c);
   writeln(c * 2.0);
   writeln(c * 0.5);
   writeln(c / d);

   {math with int and float}
   writeln(a + c);
   writeln(c - b);
   writeln(2.0 * a);
   writeln(a div 2);
   writeln(a / 2);

end.
