{
	Input :
		a = 5
		b = 10
		c = 20
		d = 4
		e = 100
	Output :
		100
		5
		100
		50
		5
		5
		15
		10
		88
		-66
		15
		-15
}

program writelnTest;
var a, b, c, d, e : integer;

begin


{READ STATEMENT}
        {single read}
        read(a);

        {multi read}
        read(b,c,d,e);  
{END READ STATEMENT}



{writeln STATEMENT}

        {writeln Factor -> unsigned int}
        writeln( 100 );

        {writeln Factor -> variable identifier}
        writeln (a);

        {writeln Factor ->"(" Expression ")" }
        writeln( (100) );

        {writeln Term -> Factor FactorTail}
                {writeln Term -> Factor      see the above operations}
                {writeln Term -> Factor MutiplyingOperator Factor FactorTail}
                        {writeln Term -> Factor "*" Factor FactorTail}
                        writeln( a * b );

                        {writeln Term -> Factor "div" Factor FactorTail}
                        writeln( c div d );

                        {writeln Term -> Factor "mod" Factor FactorTail}
                        writeln( a mod e );

	{writeln Simple Expression -> Term TermTail}
			{writeln SimpleExpression -> Term AddingOperator Term TermTail}
			{writeln SimpleExpression -> Term "+" Term TermTail}
			writeln(a + b);

			{writeln SimpleExpression -> Term "-" Term TermTail}
			writeln(c-b); 
	
	{writeln SimpleExpression -> OptionalSign Term TermTail}
		{writeln SimpleExpression -> "+" Term}
		writeln( + 88);
		
		{writeln SimpleExpression -> "-" Term}
		writeln(-66);
		
		{writeln SimpleExpression -> "+"  Term "+" Term TermTail} 
                writeln(+ a + b);

		{writeln SimpleExpression -> "-"  Term "+" Term TermTail}
                writeln(-(a + b));

	{writeln Expression -> Simple Expression OptionalRelationalPart}
		{writeln Epression -> SimpleExpression RelationalOperator SimpleExpression}
		{writeln Epression -> SimpleExpression "=" SimpleExpression}
		{writeln Epression -> SimpleExpression "<" SimpleExpression}
		{writeln Epression -> SimpleExpression ">" SimpleExpression}
		{writeln Epression -> SimpleExpression "<=" SimpleExpression}
		{writeln Epression -> SimpleExpression ">=" SimpleExpression}
		{writeln Epression -> SimpleExpression "<>" SimpleExpression}
		

{END writeln STATEMENT}
end.
