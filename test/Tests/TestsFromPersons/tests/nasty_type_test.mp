program nasty_type_test;
VAR myInt : Integer;
    myBool : Boolean;
    myFloat : Float;
BEGIN
  myInt := 1;
  myBool := True;
myFloat := 3.14;
  write('myInt value is: ');
writeln(myInt);
writeln('myBool is True');
write('myFloat is: ');
writeln(myFloat);
  writeln('Evaluting ((((( myInt * 5) * myFloat) / 2.2) >=  ((((2 + 2) div 4) * 2.25) - 1.45)) = myBool) Which should be True when all is done');
  writeln( ((((( myInt * 5) * myFloat) / 2.2) >=  ((((2 + 2) div 4) * 2.25) - 1.45)) = myBool));
END.