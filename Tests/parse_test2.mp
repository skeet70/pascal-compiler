{ a sample program 2 }
program sample2;
  var     a, b : integer;
  procedure count( n: integer; var sum: integer );
    var i: integer; { local variable }
  begin
    i := 1;
    sum := 0;
    while i <= n  do { set-up the loop }
      begin
        sum := sum + i;
        i := i + 1;
       end
  end;
begin
  read (i); { get i }
  write (); { write result }
end.