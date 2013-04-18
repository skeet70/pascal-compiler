/*************************************************************************
 * uvalues.c
 *   Functions for operating on valid values stored in the uMachine.
 *     Functions include mathematical, logical, and relational operators.
 *     A print function is also included.
 *************************************************************************/
#include <stdio.h>
#include <cmath>
#include "uvalues.h"

/*************************************************************************
 * uVALUES - MATEHMATICAL OPERATIONS(neg, add, sub, mul, div, & mod).
 *   General form is val1 op val2, returns result.
 *	  expectedType - Operator can only operate on expected type.
 *************************************************************************/

/*******************************************************************************
 * NOTE: Some of the else if statements won't ever be entered into. Specifically,
 *   when val1 == INTEGER and val2 == FLOAT. They have been left in
 *   so that they can be used in the future if machine functionality
     changes and allows FLOATS and INTEGERS to be operated on together.
 *******************************************************************************/
uVALUE negateValue(uVALUE val, TYPE expectedType)
{
    if(!validNumberType(val) || val.ElementType != expectedType)
		    val.ElementType = EMPTY;
    else if(val.ElementType == INTEGER)
        val.Element.intValue = -(val.Element.intValue);
    else if(val.ElementType == FLOAT)
        val.Element.floatValue = -(val.Element.floatValue);
    return val;
}

uVALUE addValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER){
        returnVal.ElementType = INTEGER;
        returnVal.Element.intValue = val1.Element.intValue + val2.Element.intValue;
    }
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT){	//shouldn't enter
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.intValue + val2.Element.floatValue;
    }
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER){	//shouldn't enter
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.floatValue + val2.Element.intValue;
    }
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT){
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.floatValue + val2.Element.floatValue;
    }
    return returnVal;
}

uVALUE subValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER){
        returnVal.ElementType = INTEGER;
        returnVal.Element.intValue = val1.Element.intValue - val2.Element.intValue;
    }
    else if(val1.ElementType == INTEGER &&  val2.ElementType == FLOAT){
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.intValue - val2.Element.floatValue;
    }
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER){
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.floatValue - val2.Element.intValue;
    }
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT){
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.floatValue - val2.Element.floatValue;
    }
    return returnVal;
}

uVALUE mulValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER){
        returnVal.ElementType = INTEGER;
        returnVal.Element.intValue = val1.Element.intValue * val2.Element.intValue;
    }
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT){
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.intValue * val2.Element.floatValue;
    }
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER){
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.floatValue * val2.Element.intValue;
    }
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT){
        returnVal.ElementType = FLOAT;
        returnVal.Element.floatValue = val1.Element.floatValue * val2.Element.floatValue;
    }
    return returnVal;
}

uVALUE divValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER){
        if (val2.Element.intValue == 0)
			    returnVal.ElementType = UNDEFINED;
        else
		    {
			    returnVal.ElementType = INTEGER;
        	returnVal.Element.intValue = val1.Element.intValue / val2.Element.intValue;
		    }
    }
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT){
      if (val2.Element.floatValue == 0)
			    returnVal.ElementType = UNDEFINED;
      else
		  {
			    returnVal.ElementType = FLOAT;
        	returnVal.Element.floatValue = val1.Element.intValue / val2.Element.floatValue;
    	}
	  }
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER){
        if (val2.Element.intValue == 0)
			    returnVal.ElementType = UNDEFINED;
        else
		    {
			    returnVal.ElementType = FLOAT;
        	returnVal.Element.floatValue = val1.Element.floatValue / val2.Element.intValue;
    	  }
	  }
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT){
        if (val2.Element.floatValue == 0)
			    returnVal.ElementType = UNDEFINED;
        else
		    {
		    	returnVal.ElementType = FLOAT;
        	returnVal.Element.floatValue = val1.Element.floatValue / val2.Element.floatValue;
    	  }
	  }
    return returnVal;
}

uVALUE modValues(uVALUE val1, uVALUE val2)
{
    uVALUE returnVal;
    if(!validNumberType(val1) || !validNumberType(val2))
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER){
        if (val2.Element.intValue == 0)
			    returnVal.ElementType = UNDEFINED;
		    else{
        	returnVal.ElementType = INTEGER;
          
          //C languages mod/remainder function
        	returnVal.Element.intValue = val1.Element.intValue % val2.Element.intValue;

          //Pascal Language mod definition: I mod J = I - (I div J) * J;
          //  FPC Bug: fpc 2.4.0 mod function doesn't behave correctly(as above) when I is a constant negative number. Produces incorrect results.
          //returnVal.Element.intValue = val1.Element.intValue - (val1.Element.intValue / val2.Element.intValue) * val2.Element.intValue;
		    }
    }
    else if(val1.ElementType == FLOAT || val2.ElementType == FLOAT)
		  returnVal.ElementType = EMPTY;
    return returnVal;
}


/*************************************************************************
 * uVALUES - LOGICAL OPERATORS(and, or, not).
 *   General form is val1 op val2, returns result.
 *   Returns an INTEGER value of either 0 or 1
 *************************************************************************/
uVALUE andValues(uVALUE val1, uVALUE val2)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val1) || !validNumberType(val2))
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.intValue && val2.Element.intValue;
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.intValue && val2.Element.floatValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.floatValue && val2.Element.intValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.floatValue && val2.Element.floatValue;

    return returnVal;
}

uVALUE orValues(uVALUE val1, uVALUE val2)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val1) || !validNumberType(val2))
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.intValue || val2.Element.intValue;
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.intValue || val2.Element.floatValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.floatValue || val2.Element.intValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.floatValue || val2.Element.floatValue;

    return returnVal;
}

uVALUE notValue(uVALUE val)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val))
		    returnVal.ElementType = EMPTY;
    else if(val.ElementType == INTEGER)
        returnVal.Element.intValue = !(val.Element.intValue);
    else if(val.ElementType == FLOAT)
        returnVal.Element.floatValue = !(val.Element.floatValue);
    return returnVal;
}


/*************************************************************************
 * uVALUES - RELATIONAL OPERATORS(==, >=, >, <=, <, !=).
 *   General form is val1 op val2, returns result.
 *   Returns an INTEGER value of either 0 or 1
 *************************************************************************/
uVALUE eqValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.intValue == val2.Element.intValue;
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.intValue == val2.Element.floatValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.floatValue == val2.Element.intValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.floatValue == val2.Element.floatValue;

    return returnVal;
}

uVALUE geValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.intValue >= val2.Element.intValue;
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.intValue >= val2.Element.floatValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.floatValue >= val2.Element.intValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.floatValue >= val2.Element.floatValue;

    return returnVal;
}

uVALUE gtValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.intValue > val2.Element.intValue;
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.intValue > val2.Element.floatValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.floatValue > val2.Element.intValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.floatValue > val2.Element.floatValue;

    return returnVal;
}

uVALUE leValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.intValue <= val2.Element.intValue;
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.intValue <= val2.Element.floatValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.floatValue <= val2.Element.intValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.floatValue <= val2.Element.floatValue;

    return returnVal;
}

uVALUE ltValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.intValue < val2.Element.intValue;
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.intValue < val2.Element.floatValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.floatValue < val2.Element.intValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.floatValue < val2.Element.floatValue;

    return returnVal;
}

uVALUE neValues(uVALUE val1, uVALUE val2, TYPE expectedType)
{
    uVALUE returnVal;
    returnVal.ElementType = INTEGER;

    if(!validNumberType(val1) || !validNumberType(val2) || val1.ElementType != expectedType || val2.ElementType != expectedType)
		    returnVal.ElementType = EMPTY;
    else if(val1.ElementType == INTEGER && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.intValue != val2.Element.intValue;
    else if(val1.ElementType == INTEGER && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.intValue != val2.Element.floatValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == INTEGER)
        returnVal.Element.intValue = val1.Element.floatValue != val2.Element.intValue;
    else if(val1.ElementType == FLOAT && val2.ElementType == FLOAT)
        returnVal.Element.intValue = val1.Element.floatValue != val2.Element.floatValue;

    return returnVal;
}


/*************************************************************************
 * printValue
 *   Output the element of uVALUE with no newline.
 *   If out file is stderr, prints formatted without newline.
 *************************************************************************/
void printValue(FILE* out, uVALUE val)
{
    if(val.ElementType == INTEGER && out == stderr)
        fprintf(out, "%6d", val.Element.intValue);
	  else if(val.ElementType == INTEGER && out == stdout)
		    fprintf(out, "%d", val.Element.intValue);
    else if(val.ElementType == FLOAT && out == stderr)
        fprintf(out, "%6f", val.Element.floatValue);
    else if(val.ElementType == FLOAT && out == stdout)
        fprintf(out, "%f", val.Element.floatValue);
    else if(val.ElementType == STRING && out == stderr)
        fprintf(out, "%s", val.Element.stringValue);
    else if(val.ElementType == STRING && out == stdout)
        fprintf(out, "%s", val.Element.stringValue);
    else if(val.ElementType == EMPTY && out == stderr)
        fprintf(out, " EMPTY");
    else if(val.ElementType == EMPTY && out == stdout)
        fprintf(out, "EMPTY");
    else if(val.ElementType == UNDEFINED && out == stderr)
        fprintf(out, " Undef");
    else if(val.ElementType == UNDEFINED && out == stdout)
        fprintf(out, "Undef");
}

/*************************************************************************
 * printValueLine
 *   Output the element of uVALUE with newline.
 *   If out file is stderr, prints formatted without newline.
 *************************************************************************/
void printValueLine(FILE* out, uVALUE val)
{
    if(val.ElementType == INTEGER && out == stderr)
        fprintf(out, "%6d", val.Element.intValue);
	  else if(val.ElementType == INTEGER && out == stdout)
		    fprintf(out, "%d\n", val.Element.intValue);
    else if(val.ElementType == FLOAT && out == stderr)
        fprintf(out, "%6f", val.Element.floatValue);
    else if(val.ElementType == FLOAT && out == stdout)
        fprintf(out, "%f\n", val.Element.floatValue);
    else if(val.ElementType == STRING && out == stderr)
        fprintf(out, "%s", val.Element.stringValue);
    else if(val.ElementType == STRING && out == stdout)
        fprintf(out, "%s\n", val.Element.stringValue);
    else if(val.ElementType == EMPTY && out == stderr)
        fprintf(out, " EMPTY");
    else if(val.ElementType == EMPTY && out == stdout)
        fprintf(out, "EMPTY\n");
    else if(val.ElementType == UNDEFINED && out == stderr)
        fprintf(out, " Undef");
    else if(val.ElementType == UNDEFINED && out == stdout)
        fprintf(out, "Undef\n");
}

/*************************************************************************
 * validNumberType - test if value is valid microMachine number type
 *   Return 1 if valid, 0 if type invalid(not INTEGER or FLOAT)
 *   Note: EMPTY and UNDEFINED can't be operated on so they are invalid here.
 *************************************************************************/
static int validNumberType(uVALUE val)
{
    if(val.ElementType == INTEGER || val.ElementType == FLOAT)
        return 1;
    return 0;
}

