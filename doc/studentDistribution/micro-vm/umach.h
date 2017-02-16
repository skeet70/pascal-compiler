/*************************************************************************
 * umach.h
 *   Define the uMachine enumerations, structures and operations.
 *************************************************************************/
#ifndef uMACH_H
#define uMACH_H

/* uMachine characteristics (registers, ram, code, labels) */
#define registerCount  (11)
#define stackRegister  (registerCount-1)

#define ramSize        (1024)
#define codeSize       (1024)
#define psrSize		   (1)

#define labelCount     (1024)
#define undefinedLabel (-labelCount)

#include "uvalues.h"

/*************************************************************************
 * Note: NOT CURRENTLY USED
 * Program Status Registers(PSR) bits
 *   0. COMPARE BIT  - 0 if equal, 1 if not equal 
 *-----------------------------------------------------------------------
 * Defines enums to index into the uPSR.
 ************************************************************************/
typedef enum {
	mCompare,
} uSTATUS;

/*************************************************************************
 * uCode addressing modes...
 *   -1. NONE (internal mode for error detection)
 *
 *   0. IMMEDIATE [   #n ] - a constant
 *   1. REGISTER  [   Dn ] - a register's contents
 *   2. INDEXED   [ m(Dn)] - address = Dn + m
 *   3. INDIRECT  [@m(Dn)] - address = contents of (Dn + m)
 *   4. LABEL     [   Ln ] - a label
 *------------------------------------------------------------------------
 * Stack addressing modes
 *   SP can be used to access the stack pointer in an addressing mode.
 *************************************************************************/
typedef enum {
  mNone = -1,
  mImmediate, mRegister, mIndexed, mIndirect, mLabel,
} uMODE;

/*************************************************************************
 * uCode opcodes...
 *   END instruction
 *     internal opcode for error detection
 *
 *   HALT instruction
 *     HLT               ; Terminate program execution
 *
 *   I/O instructions
 *     RD dst            ; Read an integer value into dst from STDIN
 *     RDF dst			     ; Read a float value into dst from STDIN
 *     RDS dst           ; Read a string value into dst from STDIN
 *     WRT src           ; Write the value in src to STDOUT
 *     WRTS              ; Performs  POP A  WRT A
 *     WRTLN src         ; Write the value in src followed by a newline to STDOUT
 *     WRTLNS            ; Performs  POP A  WRTLN A
 *
 *   MEMORY instructions
 *     MOV src dst       ; Performs  dst <--  src
 *    -INTEGER
 *      NEG src dst       ; Performs  dst <-- -src
 *      ADD src1 src2 dst ; Performs  dst <-- src1 + src2
 *      SUB src1 src2 dst ; Performs  dst <-- src1 - src2
 *      MUL src1 src2 dst ; Performs  dst <-- src1 x src2
 *      DIV src1 src2 dst ; Performs  dst <-- src1 / src2
 *      MOD src1 src2 dst ; Peroforms dst <-- src1 % src2
 *    -FLOAT
 *      NEGF src dst       ; Performs  dst <-- -src
 *      ADDF src1 src2 dst ; Performs  dst <-- src1 + src2
 *      SUBF src1 src2 dst ; Performs  dst <-- src1 - src2
 *      MULF src1 src2 dst ; Performs  dst <-- src1 x src2
 *      DIVF src1 src2 dst ; Performs  dst <-- src1 / src2
 *    -CASTING
 *		CASTI src dst	   ; Performs  dst <-- (int) src
 *		CASTF src dst	   ; Performs  dst <-- (float) src
 *
 *   STACK instructions
 *    PUSH src           ; Push src on top of the stack
 *    POP dst            ; Pop the top of the stack into dst
 *   -INTEGER
 *     NEGS               ; Performs  POP A  PUSH -A
 *     ADDS               ; Performs  POP A  POP B  PUSH B + A
 *     SUBS               ; Performs  POP A  POP B  PUSH B - A
 *     MULS               ; Performs  POP A  POP B  PUSH B x A
 *     DIVS               ; Performs  POP A  POP B  PUSH B / A
 *     MODS				  ; Performs  POP A  POP B  PUSH B % A
 *	 -FLOAT
 *     NEGSF               ; Performs  POP A  PUSH -A
 *     ADDSF               ; Performs  POP A  POP B  PUSH B + A
 *     SUBSF               ; Performs  POP A  POP B  PUSH B - A
 *     MULSF               ; Performs  POP A  POP B  PUSH B x A
 *     DIVSF               ; Performs  POP A  POP B  PUSH B / A
 *   -CASTING
 *     CASTSI			          ; Performs  POP A  PUSH (int)A
 *     CASTSF               ; Performs  POP A  PUSH (float)A
 *
 *   LABEL instruction
 *     Ln:               ; Drop label n at current position
 *
 *   LOGICAL OPERATOR instructions
 *     ANDS				 ; Performs  POP A  POP B  PUSH B and A
 *     ORS				 ; Performs  POP A  POP B  PUSH B or  A
 *	   NOTS				 ; Performs  POP A         PUSH   not A
 *
 *   COMPARE instructions
 *   -INTEGER
 *	   CMPEQS			 ; Performs  POP A  POP B  PUSH B =  A
 *	   CMPGES			 ; Performs  POP A  POP B  PUSH B >= A
 *	   CMPGTS			 ; Performs  POP A  POP B  PUSH B >  A
 *	   CMPLES			 ; Performs  POP A  POP B  PUSH B <= A
 *	   CMPLTS			 ; Performs  POP A  POP B  PUSH B <  A
 *	   CMPNES			 ; Performs  POP A  POP B  PUSH B <> A
 *   -FLOAT
 *     CMPEQSF            ; Performs  POP A  POP B  PUSH B =  A
 *     CMPGESF            ; Performs  POP A  POP B  PUSH B >= A
 *     CMPGTSF            ; Performs  POP A  POP B  PUSH B >  A
 *     CMPLESF            ; Performs  POP A  POP B  PUSH B <= A
 *     CMPLTSF            ; Performs  POP A  POP B  PUSH B <  A
 *     CMPNESF            ; Performs  POP A  POP B  PUSH B <> A

 *   STACK BRANCH instructions
 *	   BRTS	Ln			  ; Performs  POP A  BEQ A #1 Ln 
 *     BRFS Ln			  ; Performs  POP A  BEQ A #0 Ln
 *
 *   BRANCH instructions
 *     BR Ln			  ; Branch to label n
 *   -INTEGER 
 *     BEQ src1 src2 Ln   ; Branch to label n if src1 =  src2
 *     BGE src1 src2 Ln   ; Branch to label n if src1 >= src2
 *     BGT src1 src2 Ln   ; Branch to label n if src1 >  src2
 *     BLE src1 src2 Ln   ; Branch to label n if src1 <= src2
 *     BLT src1 src2 Ln   ; Branch to label n if src1 <  src2
 *     BNE src1 src2 Ln   ; Branch to label n if src1 <> src2
 *   -FLOAT
 *     BEQF src1 src2 Ln  ; Branch to label n if src1 =  src2
 *     BGEF src1 src2 Ln  ; Branch to label n if src1 >= src2
 *     BGTF src1 src2 Ln  ; Branch to label n if src1 >  src2
 *     BLEF src1 src2 Ln  ; Branch to label n if src1 <= src2
 *     BLTF src1 src2 Ln  ; Branch to label n if src1 <  src2
 *     BNEF src1 src2 Ln  ; Branch to label n if src1 <> src2
 *
 *   SUBROUTINE instructions
 *     CALL Ln            ; Performs  PUSH PC  BR Ln
 *     RET                ; Performs  POP PC
 *
 *   DEBUG instructions
 *     PRTS               ; Prints stack
 *     PRTR               ; Prints registers
 *************************************************************************/
typedef enum {
  oEND = -1,

  oHLT ,
  oRD  ,  oRDF , oRDS, oWRT ,  oWRTS, oWRTLN, oWRTLNS,
  oMOV ,  oNEG ,  oADD ,  oSUB ,  oMUL ,  oDIV , oMOD,
  oNEGF ,  oADDF ,  oSUBF ,  oMULF ,  oDIVF , oMODF,
  oCASTI, oCASTF,
  oPUSH,  oPOP ,  oNEGS,  oADDS,  oSUBS,  oMULS,  oDIVS, oMODS,
  oNEGSF,  oADDSF,  oSUBSF,  oMULSF,  oDIVSF,
  oCASTSI, oCASTSF,
  oLAB ,
  oANDS, oORS, oNOTS,
  oCMPEQS, oCMPGES, oCMPGTS, oCMPLES, oCMPLTS, oCMPNES,
  oCMPEQSF, oCMPGESF, oCMPGTSF, oCMPLESF, oCMPLTSF, oCMPNESF,
  oBRTS, oBRFS,
  oBR  ,  oBEQ ,  oBGE ,  oBGT ,  oBLE ,  oBLT ,  oBNE ,
  oBEQF ,  oBGEF ,  oBGTF ,  oBLEF ,  oBLTF ,  oBNEF ,
  oCALL,  oRET,
  oPRTS,  oPRTR
} uOPCODE;

/*************************************************************************
 * Operand structure (mode, register, value)
 *************************************************************************/
typedef struct {
  uMODE Mode;

  int   Register;
  //int   Value;
  uVALUE Value;
} uOPERAND;


/*************************************************************************
 * Instruction structure (source text, line number, opcode, operands)
 *************************************************************************/
typedef struct {
  char *Text;
  int   Line;

  uOPCODE  Opcode;
  uOPERAND Operand[3];
} uINSTRUCTION;

/*************************************************************************
 * Set operand value
 *************************************************************************/
int getOperandValue(uOPERAND op, uVALUE *val);
int setOperandValue(uOPERAND *op, int val);
int setOperandValue(uOPERAND *op, float val);
int setOperandValue(uOPERAND *op, char* val);
/*************************************************************************
 * uMachine reset (initialization)
 *************************************************************************/
void uReset(void);

/*************************************************************************
 * Storage access routines (registers, ram, psr and stack)
 *************************************************************************/
int psrFetch(int reg, int *val);	//Note: Not currently used...
int psrStore(int reg, int  val);	//Note: Not currently used...

int registerFetch(int reg, uVALUE *val);
int registerStore(int reg, uVALUE  val);

int ramFetch(uVALUE addr, uVALUE *val);
int ramStore(uVALUE addr, uVALUE  val);

int stackFetch(uVALUE *val);
int stackStore(uVALUE  val);

/************************************************************************
 * Outputting stack and registers - used error reporting and debugging.
 ************************************************************************/
void printStack(FILE* out);
void dumpRegisters(FILE* out);

#endif  /* uMACH_H */
