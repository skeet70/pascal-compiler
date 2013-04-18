/*************************************************************************
 * uexec.c
 *   Executing a uCode program once it has been loaded.
 *************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "umach.h"
#include "uexec.h"
#include "uconfig.h"

/*************************************************************************
 * Local routine prototypes for opcode execution.
 *************************************************************************/
static void xRD    (uINSTRUCTION *instr);
static void xRDF   (uINSTRUCTION *instr);
static void xRDS   (uINSTRUCTION *instr);
static void xWRT   (uINSTRUCTION *instr);
static void xWRTS  (uINSTRUCTION *instr);
static void xWRTLN (uINSTRUCTION *instr);
static void xWRTLNS(uINSTRUCTION *instr);

static void xMOV (uINSTRUCTION *instr);

static void xNEG (uINSTRUCTION *instr);
static void xADD (uINSTRUCTION *instr);
static void xSUB (uINSTRUCTION *instr);
static void xMUL (uINSTRUCTION *instr);
static void xDIV (uINSTRUCTION *instr);
static void xMOD (uINSTRUCTION *instr);

static void xNEGF (uINSTRUCTION *instr);
static void xADDF (uINSTRUCTION *instr);
static void xSUBF (uINSTRUCTION *instr);
static void xMULF (uINSTRUCTION *instr);
static void xDIVF (uINSTRUCTION *instr);

static void xCASTI(uINSTRUCTION *instr);
static void xCASTF(uINSTRUCTION *instr);

static void xPUSH(uINSTRUCTION *instr);
static void xPOP (uINSTRUCTION *instr);

static void xNEGS(uINSTRUCTION *instr);
static void xADDS(uINSTRUCTION *instr);
static void xSUBS(uINSTRUCTION *instr);
static void xMULS(uINSTRUCTION *instr);
static void xDIVS(uINSTRUCTION *instr);
static void xMODS(uINSTRUCTION *instr);

static void xNEGSF(uINSTRUCTION *instr);
static void xADDSF(uINSTRUCTION *instr);
static void xSUBSF(uINSTRUCTION *instr);
static void xMULSF(uINSTRUCTION *instr);
static void xDIVSF(uINSTRUCTION *instr);

static void xCASTSI(uINSTRUCTION *instr);
static void xCASTSF(uINSTRUCTION *instr);

static void xANDS(uINSTRUCTION *instr);
static void xORS(uINSTRUCTION *instr);
static void xNOTS(uINSTRUCTION *instr);

static void xCMPEQS(uINSTRUCTION *instr);
static void xCMPGES(uINSTRUCTION *instr);
static void xCMPGTS(uINSTRUCTION *instr);
static void xCMPLES(uINSTRUCTION *instr);
static void xCMPLTS(uINSTRUCTION *instr);
static void xCMPNES(uINSTRUCTION *instr);

static void xCMPEQSF(uINSTRUCTION *instr);
static void xCMPGESF(uINSTRUCTION *instr);
static void xCMPGTSF(uINSTRUCTION *instr);
static void xCMPLESF(uINSTRUCTION *instr);
static void xCMPLTSF(uINSTRUCTION *instr);
static void xCMPNESF(uINSTRUCTION *instr);

static void xBRTS  (uINSTRUCTION *instr, int label[], int *PC);
static void xBRFS  (uINSTRUCTION *instr, int label[], int *PC);

static void xBR  (uINSTRUCTION *instr, int label[], int *PC);
static void xBEQ (uINSTRUCTION *instr, int label[], int *PC);
static void xBGE (uINSTRUCTION *instr, int label[], int *PC);
static void xBGT (uINSTRUCTION *instr, int label[], int *PC);
static void xBLE (uINSTRUCTION *instr, int label[], int *PC);
static void xBLT (uINSTRUCTION *instr, int label[], int *PC);
static void xBNE (uINSTRUCTION *instr, int label[], int *PC);

static void xBEQF (uINSTRUCTION *instr, int label[], int *PC);
static void xBGEF (uINSTRUCTION *instr, int label[], int *PC);
static void xBGTF (uINSTRUCTION *instr, int label[], int *PC);
static void xBLEF (uINSTRUCTION *instr, int label[], int *PC);
static void xBLTF (uINSTRUCTION *instr, int label[], int *PC);
static void xBNEF (uINSTRUCTION *instr, int label[], int *PC);

static void xCALL(uINSTRUCTION *instr, int label[], int *PC);
static void xRET (uINSTRUCTION *instr,              int *PC);

static void xPRTS(void);
static void xPRTR(void);

/*************************************************************************
 * Local routine prototypes for operand access.
 *************************************************************************/
static void operandStore(uOPERAND op, uVALUE  val);
static void operandFetch(uOPERAND op, uVALUE *val);

static void stackPush(uVALUE  val);
static void stackPop (uVALUE *val);


/*************************************************************************
 * Local routine prototypes for execution error reporting.
 *************************************************************************/
static void execError(char *src, int line, char *mesg);

/*************************************************************************
 * "Global" execution variables...
 *************************************************************************/
char  execMessage[255] = "";

char *execText;
int   execLine;

/*************************************************************************
 * uExecute
 *   uMachine fetch & execute cycle.
 *************************************************************************/
int uExecute(uINSTRUCTION code[], int label[])
{
  uINSTRUCTION *currInstr;
  int PC = 0;

  /* Fetch & Execute cycle */
  while (code[PC].Opcode != oHLT) {
    currInstr = &code[PC];
    PC++;
    
    execText = currInstr->Text;
    execLine = currInstr->Line;

    switch (currInstr->Opcode) {
      /* CORRUPT PC - RAISE EXECUTION ERROR */
      case oEND: {
        fprintf(stderr, ">>>ERROR - DURING EXECUTION:\n");
        fprintf(stderr, "     EXECUTING INVALID CODE MEMORY\n");
        exit(1);
      }

      /* HALT instruction */
      case oHLT: { break; }

      /* I/O instructions */
      case oRD    : { xRD    (currInstr);  break; }
      case oRDF   : { xRDF   (currInstr);  break; }
      case oRDS   : { xRDS   (currInstr);  break; }
      case oWRT   : { xWRT   (currInstr);  break; }
      case oWRTS  : { xWRTS  (currInstr);  break; }
      case oWRTLN : { xWRTLN (currInstr);  break; }
      case oWRTLNS: { xWRTLNS(currInstr);  break; }

      /* Mathematical instructions */
	    /* Integer */
      case oMOV : { xMOV (currInstr);  break; }
      case oNEG : { xNEG (currInstr);  break; }
      case oADD : { xADD (currInstr);  break; }
      case oSUB : { xSUB (currInstr);  break; }
      case oMUL : { xMUL (currInstr);  break; }
      case oDIV : { xDIV (currInstr);  break; }
	    case oMOD : { xMOD (currInstr);  break; }
	    /* Float */
      case oNEGF : { xNEGF (currInstr);  break; }
      case oADDF : { xADDF (currInstr);  break; }
      case oSUBF : { xSUBF (currInstr);  break; }
      case oMULF : { xMULF (currInstr);  break; }
      case oDIVF : { xDIVF (currInstr);  break; }

      /* Stack instructions */
      case oPUSH: { xPUSH(currInstr);  break; }
      case oPOP : { xPOP (currInstr);  break; }

	    /* Stack mathematical instruction */
      /* Integer */
      case oNEGS: { xNEGS(currInstr);  break; }
      case oADDS: { xADDS(currInstr);  break; }
      case oSUBS: { xSUBS(currInstr);  break; }
      case oMULS: { xMULS(currInstr);  break; }
      case oDIVS: { xDIVS(currInstr);  break; }
      case oMODS: { xMODS(currInstr);  break; }
	    /* Float */
      case oNEGSF: { xNEGSF(currInstr);  break; }
      case oADDSF: { xADDSF(currInstr);  break; }
      case oSUBSF: { xSUBSF(currInstr);  break; }
      case oMULSF: { xMULSF(currInstr);  break; }
      case oDIVSF: { xDIVSF(currInstr);  break; }

	    /* Stack Casting instructions */
	    case oCASTSI: { xCASTSI(currInstr);  break; }
	    case oCASTSF: { xCASTSF(currInstr);  break; }

	    /* Stack logical instructions */
	    case oANDS: { xANDS(currInstr);  break; }
	    case oORS:  { xORS(currInstr);   break; }
	    case oNOTS: { xNOTS(currInstr);  break; }

	    /* Compare instructions */
	    /* Integer */
      case oCMPEQS: { xCMPEQS(currInstr);  break; }
      case oCMPGES: { xCMPGES(currInstr);  break; }
      case oCMPGTS: { xCMPGTS(currInstr);  break; }
      case oCMPLES: { xCMPLES(currInstr);  break; }
      case oCMPLTS: { xCMPLTS(currInstr);  break; }
      case oCMPNES: { xCMPNES(currInstr);  break; }
	    /* Float */
      case oCMPEQSF: { xCMPEQSF(currInstr);  break; }
      case oCMPGESF: { xCMPGESF(currInstr);  break; }
      case oCMPGTSF: { xCMPGTSF(currInstr);  break; }
      case oCMPLESF: { xCMPLESF(currInstr);  break; }
      case oCMPLTSF: { xCMPLTSF(currInstr);  break; }
      case oCMPNESF: { xCMPNESF(currInstr);  break; }

	    /* Stack branching instructions */
	    case oBRTS : { xBRTS (currInstr, label, &PC);  break; };      
	    case oBRFS : { xBRFS (currInstr, label, &PC);  break; };      
      
	    /* Label instruction - no action is performed */
      case oLAB: { break; }

      /* Braching insructions */
	    /* Integer */
      case oBR  : { xBR (currInstr, label, &PC);  break; }
      case oBEQ : { xBEQ(currInstr, label, &PC);  break; }
      case oBGE : { xBGE(currInstr, label, &PC);  break; }
      case oBGT : { xBGT(currInstr, label, &PC);  break; }
      case oBLE : { xBLE(currInstr, label, &PC);  break; }
      case oBLT : { xBLT(currInstr, label, &PC);  break; }
      case oBNE : { xBNE(currInstr, label, &PC);  break; }
      /* Float */
      case oBEQF : { xBEQF(currInstr, label, &PC);  break; }
      case oBGEF : { xBGEF(currInstr, label, &PC);  break; }
      case oBGTF : { xBGTF(currInstr, label, &PC);  break; }
      case oBLEF : { xBLEF(currInstr, label, &PC);  break; }
      case oBLTF : { xBLTF(currInstr, label, &PC);  break; }
      case oBNEF : { xBNEF(currInstr, label, &PC);  break; }
      
	    /* Subroutine instructions */
      case oCALL: { xCALL(currInstr, label, &PC);  break; }
      case oRET : { xRET (currInstr,        &PC);  break; }

      /* Debug instructions */
      case oPRTS: { xPRTS();  break; }
      case oPRTR: { xPRTR();    break; }

      /* Unknown instruction */
      default: {
        perror( ">>> INTERNAL ERROR IN uEXECUTE <<<");
        exit(1);
      }
    }

    /* Check for corrupt PC */
    if ((PC < 0) || (PC >= codeSize)) {
      fprintf(stderr, ">>>ERROR - DURING EXECUTION:\n");
      fprintf(stderr, "     EXECUTING OUTSIDE CODE MEMORY\n");
      exit(1);
    }
  }
  return 1;
}


/*************************************************************************
 * I/O instructions (rd, wrt & wrts)
 *************************************************************************/
static void xRD(uINSTRUCTION *instr)
{
  int input;
  int result;
  uVALUE val;

  if(READ_ANNOTATION)
    fprintf(stdout, "? ");
  result = fscanf(stdin , "%d", &input);
  if(result < 1){
    fprintf(stderr, ">>>ERROR - DURING EXECUTION:\n");
    fprintf(stderr, "     INVALID READ ENTRY: EXPECTED AN INTEGER.\n");
    exit(1);
  }
  
  val.ElementType = INTEGER;
  val.Element.intValue = input;
  operandStore(instr->Operand[0], val);
}

static void xRDF(uINSTRUCTION *instr)
{
  float input;
  int result;
  uVALUE val;

  if(READ_ANNOTATION)
    fprintf(stdout, "? ");
  result = fscanf(stdin , "%f", &input);
  if(result < 1){
    fprintf(stderr, ">>>ERROR - DURING EXECUTION:\n");
    fprintf(stderr, "     INVALID READ ENTRY: EXPECTED A FLOAT OR FIXED.\n");
    exit(1);
  }

  val.ElementType = FLOAT;
  val.Element.floatValue = input;
  operandStore(instr->Operand[0], val);
}

static void xRDS(uINSTRUCTION *instr)
{
  uVALUE val;
  int result;
  char* input;
  
  if(READ_ANNOTATION)
    fprintf(stdout, "? ");
  
  //Read until newline or 1023 characters(MAX_STRING_LENGTH=100 in uvalues.h)
  result = fscanf(stdin , "%1023[^\n]", val.Element.stringValue);
  //if(fgets(val.Element.stringValue, MAX_STRING_LENGTH, stdin) == NULL){
  if(result < 1){
    fprintf(stderr, ">>>ERROR - DURING EXECUTION:\n");
    fprintf(stderr, "     INVALID READ ENTRY: EXPECTED A STRING WITH A LENGTH OF AT LEAST 1.\n");
    exit(1);
  }
  val.ElementType = STRING;
  operandStore(instr->Operand[0], val);
}

static void xWRT(uINSTRUCTION *instr)
{
  uVALUE val;

  operandFetch(instr->Operand[0], &val);
  printValue(stdout, val);
}

static void xWRTS(uINSTRUCTION *instr)
{
  uVALUE val;
  stackPop(&val);
  printValue(stdout, val);
}

static void xWRTLN(uINSTRUCTION *instr)
{
  uVALUE val;

  operandFetch(instr->Operand[0], &val);
  printValueLine(stdout, val);
}

static void xWRTLNS(uINSTRUCTION *instr)
{
  uVALUE val;

  stackPop(&val);
  printValueLine(stdout, val);
}

/*************************************************************************
 * Memory instructions (mov, neg, add, sub, mul & div)
 *************************************************************************/
static void xMOV(uINSTRUCTION *instr)
{
  uVALUE val;

  operandFetch(instr->Operand[0], &val);
  operandStore(instr->Operand[1],  val);
}

static void xNEG(uINSTRUCTION *instr)
{
  uVALUE val;

  operandFetch(instr->Operand[0], &val);
  val = negateValue(val, INTEGER);
  if(val.ElementType == EMPTY)
  {
	sprintf(execMessage, "Negation integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  operandStore(instr->Operand[1], val);
}


static void xADD(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);
  val1 = addValues(val1, val2, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Addition integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  operandStore(instr->Operand[2], val1);
}

static void xSUB(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);
  val1 = subValues(val1, val2, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Subtraction integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  operandStore(instr->Operand[2], val1); //val1 - val2
}

static void xMUL(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);
  val1 = mulValues(val1, val2, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Muliplication integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  operandStore(instr->Operand[2], val1);
}

static void xDIV(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  val1 = divValues(val1, val2, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Division integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  else if(val1.ElementType == UNDEFINED)
  {
	sprintf(execMessage, "DIVSION BY ZERO.");
  	execError(execText, execLine, execMessage);
  }

  operandStore(instr->Operand[2], val1);	//val1 / val2
}

static void xMOD(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  val1 = modValues(val1, val2);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Modulo operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  else if(val1.ElementType == UNDEFINED)
  {
	sprintf(execMessage, "Modulo operation: DIVISION BY ZERO.");
  	execError(execText, execLine, execMessage);
  }
  
  operandStore(instr->Operand[2], val1);
}

static void xNEGF (uINSTRUCTION *instr)
{
  uVALUE val;

  operandFetch(instr->Operand[0], &val);
  val = negateValue(val, FLOAT);
  if(val.ElementType == EMPTY)
  {
	sprintf(execMessage, "Negation float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  operandStore(instr->Operand[1], val);
}

static void xADDF (uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);
  val1 = addValues(val1, val2, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Addition float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  operandStore(instr->Operand[2], val1);
}

static void xSUBF (uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);
  val1 = subValues(val1, val2, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Subtraction float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  operandStore(instr->Operand[2], val1); //val1 - val2
}

static void xMULF (uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);
  val1 = mulValues(val1, val2, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Muliplication float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  operandStore(instr->Operand[2], val1);
}

static void xDIVF (uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  val1 = divValues(val1, val2, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Division float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  else if(val1.ElementType == UNDEFINED)
  {
	sprintf(execMessage, "DIVSION BY ZERO.");
  	execError(execText, execLine, execMessage);
  }

  operandStore(instr->Operand[2], val1);	//val1 / val2
}

/*********************************************************************
 * Memory cast instructions(casti & castj)
 *   Cast from float to int and vice versa
 ********************************************************************/
static void xCASTI (uINSTRUCTION *instr)
{
  uVALUE val;

  operandFetch(instr->Operand[0], &val);
  
  if(val.ElementType == EMPTY)
  {
	sprintf(execMessage, "Trying to cast an empty value.");
	execError(execText, execLine, execMessage);
  } 
  else if(val.ElementType == FLOAT)
  {
	val.ElementType = INTEGER;
	val.Element.intValue = (int)val.Element.floatValue;
  }
  else if(val.ElementType == INTEGER)
  {
	//Do nothing, already integer
  }
  
  operandStore(instr->Operand[1], val);
}

static void xCASTF (uINSTRUCTION *instr)
{
  uVALUE val;

  operandFetch(instr->Operand[0], &val);
  
  if(val.ElementType == EMPTY)
  {
	sprintf(execMessage, "Trying to cast an empty value.");
	execError(execText, execLine, execMessage);
  } 
  else if(val.ElementType == INTEGER)
  {
	val.ElementType = FLOAT;
	val.Element.intValue = (float)val.Element.intValue;
  }
  else if(val.ElementType == FLOAT)
  {
	//Do nothing, already integer
  }
 
  operandStore(instr->Operand[1], val);
}

/*************************************************************************
 * Stack instructions (push, pop, adds, subs, muls, divs, & mods)
 *************************************************************************/
static void xPUSH(uINSTRUCTION *instr)
{
  uVALUE val;
  operandFetch(instr->Operand[0], &val);
  stackPush(val);
}

static void xPOP(uINSTRUCTION *instr)
{
  uVALUE val;

  stackPop(&val);
  operandStore(instr->Operand[0], val);
}


static void xNEGS(uINSTRUCTION *instr)
{
  uVALUE val;

  stackPop(&val);
  val = negateValue(val, INTEGER);
  if(val.ElementType == EMPTY)
  {
	sprintf(execMessage, "Negation integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val);
}

static void xADDS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = addValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Addition integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xSUBS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = subValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Subtraction integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);	//val2 - val1
}

static void xMULS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = mulValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Multiplication integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xDIVS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = divValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Division integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  else if(val1.ElementType == UNDEFINED)
  {
	sprintf(execMessage, "DIVSION BY ZERO.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);	//val2 / val1
}

static void xMODS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;
  
  stackPop(&val1);
  stackPop(&val2);
  val1 = modValues(val2, val1);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Modulo operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  else if(val1.ElementType == UNDEFINED)
  {
	sprintf(execMessage, "Modulo operation: DIVISION BY ZERO.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xNEGSF(uINSTRUCTION *instr)
{
  uVALUE val;

  stackPop(&val);
  val = negateValue(val, FLOAT);
  if(val.ElementType == EMPTY)
  {
	sprintf(execMessage, "Negation float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val);
}

static void xADDSF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = addValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Addition float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xSUBSF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = subValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Subtraction float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);	//val2 - val1
}

static void xMULSF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = mulValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Multiplication float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xDIVSF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = divValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Division float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  else if(val1.ElementType == UNDEFINED)
  {
	sprintf(execMessage, "DIVSION BY ZERO.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);	//val2 / val1
}

/*************************************************************************
 * Stack casting instructions (castsi, castsf)
 *************************************************************************/
static void xCASTSI(uINSTRUCTION *instr)
{
  uVALUE val1;

  stackPop(&val1);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Trying to cast an empty value.");
	execError(execText, execLine, execMessage);
  } 
  else if(val1.ElementType == FLOAT)
  {
	val1.ElementType = INTEGER;
	val1.Element.intValue = (int)val1.Element.floatValue;
  }
  else if(val1.ElementType == INTEGER)
  {
	//Do nothing, already integer
  }
  else if(val1.ElementType == STRING)
  {
  sprintf(execMessage, "Trying to cast a string to a float.");
  execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}
static void xCASTSF(uINSTRUCTION *instr)
{
  uVALUE val1;

  stackPop(&val1);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Trying to cast an empty value.");
	execError(execText, execLine, execMessage);
  } 
  else if(val1.ElementType == INTEGER)
  {
	val1.ElementType = FLOAT;
	val1.Element.floatValue = (float)val1.Element.intValue;
  }
  else if(val1.ElementType == FLOAT)
  {
	//Do nothing, already float
  }
  else if(val1.ElementType == STRING)
  {
  sprintf(execMessage, "Trying to cast a string to an integer.");
  execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

/************************************************************************
 * Stack logical instructions(ands, ors, & nots)
 *    0 is false, 1 is true
 *************************************************************************/
static void xANDS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = andValues(val2, val1);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Logical AND operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xORS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = orValues(val2, val1);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Logical OR operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xNOTS(uINSTRUCTION *instr)
{
  uVALUE val1;

  stackPop(&val1);
  val1 = notValue(val1);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Logical NOT operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

/************************************************************************
 * Compare instructions(cmpeqs, cmpges, cmpgts, cmples, cmplts, & cmpnes)
 *    0 is false, 1 is true
 *************************************************************************/
static void xCMPEQS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = eqValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational EQUALS integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPGES(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = geValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational GREATER THAH/EQUALS TO integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPGTS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = gtValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational GREATER THAN integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPLES(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = leValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational LESS THAN/EQUALS TO integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPLTS(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = ltValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational LESS THAN integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPNES(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = neValues(val2, val1, INTEGER);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational NOT EQUALS integer operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPEQSF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = eqValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational EQUALS float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPGESF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = geValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational EQUALS float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPGTSF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = gtValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational EQUALS float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPLESF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = leValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational EQUALS float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPLTSF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = ltValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational EQUALS float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

static void xCMPNESF(uINSTRUCTION *instr)
{
  uVALUE val1;
  uVALUE val2;

  stackPop(&val1);
  stackPop(&val2);
  val1 = neValues(val2, val1, FLOAT);
  if(val1.ElementType == EMPTY)
  {
	sprintf(execMessage, "Relational EQUALS float operation on invalid TYPE.");
  	execError(execText, execLine, execMessage);
  }
  stackPush(val1);
}

/*************************************************************************
 * Stack branch instructions (brts & brfs)
 *************************************************************************/
static void xBRTS(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE lab;
  uVALUE one;	//stores constant 1

  getOperandValue(instr->Operand[0], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  stackPop(&val1);

  one.ElementType = INTEGER;
  one.Element.intValue = 1;
  
  if (eqValues(val1, one, INTEGER).Element.intValue) {	
    *PC = label[lab.Element.intValue];
  }
}

static void xBRFS(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE lab;
  uVALUE zero;	//stores constant 0

  getOperandValue(instr->Operand[0], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  stackPop(&val1);

  zero.ElementType = INTEGER;
  zero.Element.intValue = 0;
  if (eqValues(val1, zero, INTEGER).Element.intValue) {	
    *PC = label[lab.Element.intValue];
  }
}

/*************************************************************************
 * Branch instructions (br, beq, bge, bgt, ble, blt & bne)
 *************************************************************************/
static void xBR(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE lab;

  getOperandValue(instr->Operand[0], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  *PC = label[lab.Element.intValue];
}

static void xBEQ(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (eqValues(val1, val2, INTEGER).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBGE(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf( execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError( execText, execLine, execMessage);
  }

  if (geValues(val1, val2, INTEGER).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBGT(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf( execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError( execText, execLine, execMessage);
  }

  if (gtValues(val1, val2, INTEGER).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBLE(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf( execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError( execText, execLine, execMessage);
  }

  if (leValues(val1, val2, INTEGER).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBLT(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (ltValues(val1, val2, INTEGER).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBNE(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (neValues(val1, val2, INTEGER).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBEQF (uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (eqValues(val1, val2, FLOAT).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBGEF (uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (geValues(val1, val2, FLOAT).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBGTF (uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (gtValues(val1, val2, FLOAT).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBLEF (uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (leValues(val1, val2, FLOAT).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBLTF (uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (ltValues(val1, val2, FLOAT).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

static void xBNEF (uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE val1;
  uVALUE val2;
  uVALUE lab;

  operandFetch(instr->Operand[0], &val1);
  operandFetch(instr->Operand[1], &val2);

  getOperandValue(instr->Operand[2], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  if (neValues(val1, val2, FLOAT).Element.intValue) {
    *PC = label[lab.Element.intValue];
  }
}

/*************************************************************************
 * Subroutine instructions (call & ret)
 *************************************************************************/
static void xCALL(uINSTRUCTION *instr, int label[], int *PC)
{
  uVALUE lab;
  uVALUE pc;

  getOperandValue(instr->Operand[0], &lab);

  if (lab.ElementType != INTEGER){
	sprintf(execMessage, "Label number must be an integer");
	execError(execText, execLine, execMessage);
  }
  else if (label[lab.Element.intValue] == undefinedLabel) {
    sprintf(execMessage, "UNDEFINED LABEL L%d", lab.Element.intValue);
    execError(execText, execLine, execMessage);
  }

  pc.ElementType = INTEGER;
  pc.Element.intValue = *PC;
  stackPush(pc);	//stackPush, FLOAT(*PC);
  *PC = label[lab.Element.intValue];
}

static void xRET(uINSTRUCTION *instr, int *PC)
{
  uVALUE pc;
  stackPop(&pc);	//stackPop(PC);
  *PC = pc.Element.intValue;
}

/*************************************************************************
 * Debug instructions (print stack and print registers)
 *************************************************************************/
static void xPRTS(void)
{
  printStack(stdout);
}
static void xPRTR(void)
{
  dumpRegisters(stdout);
}
/*************************************************************************
 * Operand processing routines (with error detection)
 *************************************************************************/
static void operandFetch(uOPERAND op, uVALUE *val)
{
  uVALUE addr, offset;

  switch (op.Mode) {
    case mNone: {
      execError(execText, execLine, "MISSING SOURCE OPERAND");
      break;
    }

    case mImmediate: {
	    getOperandValue(op, val);
      break;
    }

    case mRegister: {
      if (!registerFetch(op.Register, val)) {
        sprintf(execMessage, "ACCESS FAILURE - D%d", op.Register);
        execError(execText, execLine, execMessage);
      }

      break;
    }

    case mIndexed: {
      if (!registerFetch(op.Register, &addr)) {
        sprintf(execMessage, "ACCESS FAILURE - D%d", op.Register);
        execError(execText, execLine, execMessage);
      }

	    getOperandValue(op, &offset);
	    addr.Element.intValue += offset.Element.intValue;

      if (!ramFetch(addr, val)) {
        sprintf(execMessage, "ACCESS FAILURE - %d(D%d)", op.Value, op.Register);
        execError(execText, execLine, execMessage);
      }

      break;
    }

    case mIndirect: {
      if (!registerFetch(op.Register, &addr)) {
        sprintf(execMessage, "ACCESS FAILURE - D%d", op.Register);
        execError(execText, execLine, execMessage);
      }

	  getOperandValue(op, &offset);
	  addr.Element.intValue += offset.Element.intValue;

      if (!ramFetch(addr, &addr)) {
        sprintf(execMessage, "ACCESS FAILURE - %d(D%d)", op.Value, op.Register);
        execError(execText, execLine, execMessage);
      }
	  addr.Element.intValue;
      
      if (!ramFetch(addr, val)) {
        sprintf(execMessage, "ACCESS FAILURE - @%d(D%d)", op.Value, op.Register);
        execError(execText, execLine, execMessage);
      }

      break;
    }

    /* A label cannot be used as a source operand */
    case mLabel: {
      sprintf(execMessage, "L%d CANNOT BE A SOURCE OPEAND", op.Value);
      execError(execText, execLine, execMessage);
      break;
    }

    default: {
      perror(">>> INTERNAL ERROR IN FETCHOPERAND <<<");
      exit(1);
    }
  }
}

static void operandStore(uOPERAND op, uVALUE val)
{
  uVALUE addr, offset;

  switch (op.Mode) {
    case mNone: {
      execError(execText, execLine, "MISSING TARGET OPERAND");
      break;
    }

    case mImmediate: {
      sprintf(execMessage, "#%d CANNOT BE A TARGET OPERAND", op.Value);
      execError(execText, execLine, execMessage);
      break;
    }

    case mRegister: {
      if (!registerStore(op.Register, val)) {
        sprintf(execMessage, "STORAGE FAILURE - D%d", op.Register);
        execError(execText, execLine, execMessage);
      }

      break;
    }

    case mIndexed: {
      if (!registerFetch(op.Register, &addr)) {
        sprintf(execMessage, "ACCESS FAILURE - D%d", op.Register);
        execError(execText, execLine, execMessage);
      }

	  getOperandValue(op, &offset);
	  addr.Element.intValue += offset.Element.intValue;

      if (!ramStore(addr, val)) {
        sprintf(execMessage, "STORAGE FAILURE - %d(D%d)", op.Value, op.Register);
        execError(execText, execLine, execMessage);
      }

      break;
    }

    case mIndirect: {
      if (!registerFetch(op.Register, &addr)) {
        sprintf(execMessage, "ACCESS FAILURE - D%d", op.Register);
        execError(execText, execLine, execMessage);
      }
	  
	  getOperandValue(op, &offset);
	  addr.Element.intValue += offset.Element.intValue;

      if (!ramFetch(addr, &addr)) {
        sprintf(execMessage, "ACCESS FAILURE - %d(D%d)", op.Value, op.Register);
        execError(execText, execLine, execMessage);
      }

      if (!ramStore(addr, val)) {
        sprintf(execMessage, "STORAGE FAILURE - @%d(D%d)", op.Value, op.Register);
        execError(execText, execLine, execMessage);
      }

      break;
    }

    /* A label cannot be used as a target operand */
    case mLabel: {
      sprintf(execMessage, "L%d CANNOT BE A TARGET OPERAND", op.Value);
      execError(execText, execLine, execMessage);
      break;
    }

    default: {
      perror(">>> INTERNAL ERROR IN STOREOPERAND <<<");
      exit(1);
    }
  }
}

/*************************************************************************
 * stackPush & stackPop
 *   Push & Pop wrappers for error reporting.
 *************************************************************************/
static void stackPush(uVALUE val)
{
  if (!stackStore(val))
    execError(execText, execLine, "STACK OVERFLOW");
}

static void stackPop(uVALUE *val)
{
  if (!stackFetch(val))
    execError(execText, execLine, "STACK UNDERFLOW");
}


/*************************************************************************
 * execError
 *   Report an error during program execution.
 *************************************************************************/
static void execError(char *text, int line, char *mesg)
{
  fprintf(stderr, ">>>ERROR DURING EXECUTION: %s\n", mesg);
  fprintf(stderr, "     LINE %d: %s\n", line, text);
  fprintf(stderr, "\n");
  dumpRegisters(stderr);
  exit(1);
}
