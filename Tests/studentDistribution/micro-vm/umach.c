/*************************************************************************
 * umach.c
 *   uMachine emulator (hardware + access routines).
 *************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "umach.h"


/*************************************************************************
 * uMachine hardware
 *   Define registers, ram, code, program status register(PSR), and labels.
 *************************************************************************/
uVALUE       uRegister[registerCount];
uVALUE       uRAM     [ramSize];
int			     uPSR	    [psrSize];	//Note: Not currently used...
uINSTRUCTION uCode    [codeSize];
int          uLabel   [labelCount];

/*************************************************************************
 * setOperandValue
 * 	Set operand value based upon its type.
 *************************************************************************/
int getOperandValue(uOPERAND op, uVALUE *val)
{
	if(op.Value.ElementType == EMPTY) return 0;

	*val = op.Value;
	return 1;
}
int setOperandValue(uOPERAND *op, int val)
{
  if(op->Value.ElementType == STRING){
    //free(op->Value.Element.stringValue);
	}
  
  op->Value.ElementType = INTEGER;
	op->Value.Element.intValue = val;
	return 1;
}
int setOperandValue(uOPERAND *op, float val)
{
  if(op->Value.ElementType == STRING){
    //free(op->Value.Element.stringValue);
	}
  op->Value.ElementType = FLOAT;
	op->Value.Element.floatValue = val;
	return 1;
}
int setOperandValue(uOPERAND *op, char* val)
{
	op->Value.ElementType = STRING;
	//op->Value.Element.stringValue = (char*)malloc((strlen(val))*sizeof(char));
  //if(op->Value.Element.stringValue == NULL){
  //  fprintf(stderr, "VM out of memory: Couldn't allocate memory for the string\n");
  //  exit(EXIT_FAILURE);
  //}
	strncpy(op->Value.Element.stringValue, val, MAX_STRING_LENGTH);
	return 1;
}
/*************************************************************************
 * uReset
 *   Reset the uMachine hardware.
 *************************************************************************/
void uReset(void)
{
  int i;

  /* Clear registers and initialize the stack register */
  for (i = 0; i < registerCount; i++) {
	uRegister[i].ElementType = EMPTY;
	uRegister[i].Element.intValue = -1;
  }

  uRegister[stackRegister].ElementType = INTEGER;
  uRegister[stackRegister].Element.intValue = 0;

  /* Clear ram */
  for (i = 0; i < ramSize; i++) {
    if(uRAM[i].ElementType == STRING){  //If String, free malloc'ed memory.
      //free(uRAM[i].Element.stringValue);
    }
	uRAM[i].ElementType = EMPTY;
	uRAM[i].Element.intValue = -1;
  }

  /* Clear code space */
  for (i = 0; i < codeSize; i++) {
    uCode[i].Text = (char *) NULL;
    uCode[i].Line = -1;

    uCode[i].Opcode = oEND;

    uCode[i].Operand[0].Mode     = mNone;
    uCode[i].Operand[0].Register = -1;
    uCode[i].Operand[0].Value.ElementType = EMPTY;
    uCode[i].Operand[0].Value.Element.intValue = -1;

    uCode[i].Operand[1].Mode     = mNone;
    uCode[i].Operand[1].Register = -1;
    uCode[i].Operand[1].Value.ElementType = EMPTY;
    uCode[i].Operand[1].Value.Element.intValue = -1;

    uCode[i].Operand[2].Mode     = mNone;
    uCode[i].Operand[2].Register = -1;
    uCode[i].Operand[2].Value.ElementType = EMPTY;
    uCode[i].Operand[2].Value.Element.intValue = -1;
  }

  /* Clear PSR "bits" */
  for (i = 0; i < psrSize; i++){
	uPSR[i] = 0;
  }

  /* Clear label space */
  for (i = 0; i < labelCount; i++) {
    uLabel[i] = undefinedLabel;
  }
}


/*************************************************************************
 * Note: Not currently used..
 * psrFetch (return: 0 - failure; 1 - success)
 *   Fetch a value from PSR.
 *
 * psrStore (return: 0 - failure; 1 - success)
 *   Store a value into the PSR.
 *************************************************************************/
int psrFetch(int reg, int *val){
	if((reg < 0) || (reg >= psrSize)) return 0;

	*val = uPSR[reg];
	return 1;
}

int psrStore(int reg, int val)
{
  if ((reg < 0) || (reg >= psrSize))  return 0;

  uPSR[reg] = val;
  return 1;
}

/*************************************************************************
 * registerFetch (return: 0 - failure; 1 - success)
 *   Fetch a value from a register.
 *
 * registerStore (return: 0 - failure; 1 - success)
 *   Store a value into a register.
 *************************************************************************/
int registerFetch(int reg, uVALUE *val)
{
  if ((reg < 0) || (reg >= registerCount))  return 0;

  *val = uRegister[reg];
  return 1;
}

int registerStore(int reg, uVALUE val)
{
  if ((reg < 0) || (reg >= registerCount))  return 0;

  if(reg == stackRegister && val.ElementType != INTEGER)
  {
	fprintf(stderr, "registerStore error: Trying to store non-integer into stack register.\n");
	return 0;
  }
  uRegister[reg] = val;
  return 1;
}

/*************************************************************************
 * ramFetch (return: 0 - failure; 1 - success)
 *   Fetch a value from a memory address.
 *
 * ramStore (return: 0 - failure; 1 - success)
 *   Store a value into a memory address.
 *************************************************************************/
int ramFetch(uVALUE addr, uVALUE *val)
{
  if(addr.ElementType != INTEGER)
  {
	fprintf(stderr, "ramFetch error: Attempting to use a non-integer as an address.\n");
	return 0;
  }
  if ((addr.Element.intValue < 0) || (addr.Element.intValue >= ramSize))  return 0;
  
  *val = uRAM[addr.Element.intValue];
  return 1;
}

int ramStore(uVALUE addr, uVALUE val)
{
  if(addr.ElementType != INTEGER)
  {
	fprintf(stderr, "ramStore error: Attempting to use a non-integer as an address.\n");
	return 0;
  } 
  if ((addr.Element.intValue < 0) || (addr.Element.intValue >= ramSize))  return 0;

  uRAM[addr.Element.intValue] = val;
  return 1;
}


/*************************************************************************
 * stackFetch (return: 0 - failure; 1 - success)
 *   Fetch a value from the top of the stack (pop).
 *
 * stackStore (return: 0 - failure; 1 - success)
 *   Store a value on top of the stack (push).
 *************************************************************************/
int stackFetch(uVALUE *val)
{
  uVALUE addr;

  if(uRegister[stackRegister].ElementType != INTEGER)
  {
	fprintf(stderr, "VM broken: registerFetch error: Stack register has a non-integer in stack register.\n");
	return 0;
  }

  addr.ElementType = INTEGER;
  addr.Element.intValue = --(uRegister[stackRegister].Element.intValue);
  return ramFetch(addr, val);
}

int stackStore(uVALUE val)
{
  uVALUE addr;

  if(uRegister[stackRegister].ElementType != INTEGER)
  {
	fprintf(stderr, "registerStore error: Trying to store non-integer in stack register.\n");
	return 0;
  }

  addr.ElementType = INTEGER;
  addr.Element.intValue = (uRegister[stackRegister].Element.intValue)++;
  return ramStore(addr, val);
}

void printStack(FILE* out)
{
  if(uRegister[stackRegister].ElementType != INTEGER) {
	  fprintf(stderr, "VM broken: printStack error: Stack register has a non-integer in stack register.\n");
    return;
  }
  int SP = uRegister[stackRegister].Element.intValue;
  fprintf(out, "---------STACK---------\n");
  fprintf(out, "ADDR  VALUE\n");
  fprintf(out, "%04d  ", SP);
  printValue(out, uRAM[SP]);
  fprintf(out, " <--SP\n");
  for(int addr = SP-1; addr >= 0; addr--) {
    fprintf(out, "%04d  ", addr);
    printValueLine(out, uRAM[addr]);
  }
  fprintf(out, "-----------------------\n");
}
void dumpRegisters(FILE* out)
{
  int reg;
  uVALUE val;

  fprintf(out, "-----REGISTER DUMP-----\n");

  for (reg = 0; reg < registerCount-1; reg++) {
   fprintf(out, "D%1d: ", reg);
   registerFetch(reg, &val);
   printValueLine(out, val);
  }

  fprintf(out, "SP: ");
  registerFetch(stackRegister, &val);
  printValueLine(out, val);

  fprintf(out, "-----------------------\n");
}

