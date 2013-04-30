{For loop test
	Input:
	10
	Output:
	1 - 10
	20 - 10
}

program forTest;

    var I: Integer;
      N: Integer;

  begin
    Read(N);
    for I := 1 to N do
      Writeln(I);

    for I := 20 downto N do
      Writeln(I);
 
  end.
