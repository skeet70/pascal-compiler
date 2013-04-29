/*************************************************************************
 * uvalue.h
 *   Defines the uVALUE enumeration, structures, and operations. 
 *      uVALUE is necessary to store both floats and integers on the stack.
 *   This header is directly included in umach.h.
 *************************************************************************/
#ifndef uVALUES_H
#define uVALUES_H

#define MAX_STRING_LENGTH (1024)

#include <cstring>
/*************************************************************************
 * uVALUE types (FLOAT, INTEGER) currently supported
 *	 -1. EMPTY: Used as a placeholder for original stack/ram/reg declarations.
 *	  	    Later used to indicate errors(check if type EMPTY)
 *          Note: Stack IS NOT reset to EMPTY every time it is popped.
 *	  0. UNDEFINED: DIV by zero, MOD operator on FLOAT, etc.
 *    1. INTEGER:   Standard integer type.
 *    2. FLOAT:     Both fixed and float types in uMachine. 
 *    3. STRING:    String literal that contains no newlines.
 *    4. BOOLEAN:   bool false(0) or true(1) ---NOT USED
 *************************************************************************/
typedef enum {
    EMPTY = -1, UNDEFINED, INTEGER, FLOAT, STRING, BOOLEAN
} TYPE;


/*************************************************************************
 * uVALUES - Types of values micromachine can store on stack.
 *   "Tagged Union structure"
 *    ElementType: Tag which indicates type defined(INTEGER OR FLOAT)
 *	  Element: uVALUE can only store an int or a float, not both.
 *
 *    Note: If TYPE is EMPTY or UNDEFINED, neither are defined.
 *************************************************************************/
typedef struct {
  TYPE ElementType;		//Tag: enumerated type
  union {				//Only one defined at a time.
    int intValue;
    float floatValue;
	  char stringValue[MAX_STRING_LENGTH];	//Memory link, never free'd when popped...
  }Element;
} uVALUE;

/*************************************************************************
 * Local routine prototypes for uVALUE math operations.
 *************************************************************************/
uVALUE negateValue(uVALUE val, TYPE expectedType);
uVALUE addValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE subValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE mulValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE divValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE modValues(uVALUE val1, uVALUE val2);

/*************************************************************************
 * Local routine prototypes for uVALUE logical operators.
 *************************************************************************/
uVALUE andValues(uVALUE val1, uVALUE val2);
uVALUE orValues (uVALUE val1, uVALUE val2);
uVALUE notValue (uVALUE val);

/*************************************************************************
 * Local routine prototypes for uVALUE relational operators.
 *************************************************************************/
uVALUE eqValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE geValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE gtValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE leValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE ltValues(uVALUE val1, uVALUE val2, TYPE expectedType);
uVALUE neValues(uVALUE val1, uVALUE val2, TYPE expectedType);

/*************************************************************************
 * Local routine prototypes for printing uVALUES and testing their validity
 *************************************************************************/
void printValue(FILE* out, uVALUE val);
void printValueLine(FILE* out, uVALUE val);
static int validNumberType(uVALUE val);

#endif /* uVALUES_H */
