/*************************************************************************
 * uload.c
 *   Load (+ parse) a uCode source file.
 *************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "umach.h"
#include "uscan.h"
#include "uload.h"


/*************************************************************************
 * Status values
 *************************************************************************/
#define valid           (0)
#define invalid         (1)

#define invalidOpcode   (2)
#define invalidRegister (3)
#define invalidLabel    (4)
#define invalidForm     (5)
#define badLabel        (6)
#define invalidString   (7)

/*************************************************************************
 * Local routine prototypes for instruction processing.
 *************************************************************************/
static uTOKEN  processWhitespace(void);

static void    clearInstruction(uINSTRUCTION *instr);
static void    fillInformation (uINSTRUCTION *instr, char *text, int line);

static void    loadOpcode(uINSTRUCTION *instr);
static void    loadSource(uINSTRUCTION *instr, int opNum);
static void    loadTarget(uINSTRUCTION *instr, int opNum);
static void    loadLabel (uINSTRUCTION *instr, int opNum);
static void    loadEnd   (void);

static void    validateRegister(int reg);
static void    validateLabel   (int lab);

static void    loadError(char *text, int line, char *mesg);
static void    printCorrectForm(uOPCODE Opcode);

static void    replaceEscapeSequences(char* s, char* parsedString);  

/*************************************************************************
 * Flex interface
 *************************************************************************/
extern int       yylex();
extern char     *yytext;
extern uTOKEN    yytok;


/*************************************************************************
 * "Global" execution variables
 *************************************************************************/
char  loadMessage[255] = "";

char  loadText[255] = "";
int   loadLine      = 1;

int   loadFlag = valid;
int   codeFlag = valid;

/*************************************************************************
 * uLoad
 *   "Parse" a uCode source program (using flex to scan it).
 *************************************************************************/
int uLoad(uINSTRUCTION code[], int label[])
{
  uINSTRUCTION *currInstr;
  int codeLine = 0;

  int i, lab;

  do {
    currInstr = &(code[codeLine]);
    clearInstruction(currInstr);
    codeFlag = valid;
    loadOpcode(currInstr);
    if (codeFlag == valid) {
      switch (yytok) {
        case tHLT:
          break;

        case tRD:
        case tRDF:
        case tRDS:
          loadTarget(currInstr, 0);
          break;

        case tWRT:
        case tWRTLN:
          loadSource(currInstr, 0);
          break;

        case tWRTS:
        case tWRTLNS:
          break;

        case tMOV  :
        case tNEG  :
        case tNEGF :
        case tCASTI:
        case tCASTF:
          loadSource(currInstr, 0);
          loadTarget(currInstr, 1);
          break;

        case tADD :
        case tSUB :
        case tMUL :
        case tDIV :
        case tMOD :
        case tADDF :
        case tSUBF :
        case tMULF :
        case tDIVF :
          loadSource(currInstr, 0);
          loadSource(currInstr, 1);
          loadTarget(currInstr, 2);
          break;

        case tPUSH:
          loadSource(currInstr, 0);
          break;

        case tPOP :
          loadTarget(currInstr, 0);
          break;

        case tNEGS:
        case tADDS:
        case tSUBS:
        case tMULS:
        case tDIVS:
        case tMODS:
        case tNEGSF:
        case tADDSF:
        case tSUBSF:
        case tMULSF:
        case tDIVSF:
        case tCASTSI:
        case tCASTSF:
          break;

        /* Label instruction must be handled at load time */
        case tLAB :
        {
          sscanf(yytext, "%*[Ll]%d:", &lab);

          currInstr->Operand[0].Mode  = mLabel;
          setOperandValue(&(currInstr->Operand[0]), lab); 
          validateLabel(lab);

          if (codeFlag == valid) {
            if (label[lab] != undefinedLabel) {
              sprintf(loadMessage, "L%d REDEFINED", lab);
              codeFlag = badLabel;
            } else {
              label[lab] = codeLine;
            }
          }

          break;
        }
    
        case tANDS:
        case tORS:
        case tNOTS:
          break;

        case tCMPEQS:
        case tCMPGES:
        case tCMPGTS:
        case tCMPLES:
        case tCMPLTS:
        case tCMPNES:
        case tCMPEQSF:
        case tCMPGESF:
        case tCMPGTSF:
        case tCMPLESF:
        case tCMPLTSF:
        case tCMPNESF:
          break;

        case tBRTS:
        case tBRFS:
          loadLabel(currInstr, 0);
          break;

        case tBR  :
          loadLabel(currInstr, 0);
          break;

        case tBEQ :
        case tBGE :
        case tBGT :
        case tBLE :
        case tBLT :
        case tBNE :
        case tBEQF :
        case tBGEF :
        case tBGTF :
        case tBLEF :
        case tBLTF :
        case tBNEF :
          loadSource(currInstr, 0);
          loadSource(currInstr, 1);
          loadLabel (currInstr, 2);
          break;

        case tCALL:
          loadLabel(currInstr, 0);
          break;

        case tRET :
          break;

        case tEOF :
          break;

        case tPRTS:
        case tPRTR:
          break;

        default:
          perror(">>> INTERNAL ERROR IN LOAD_uCODE <<<");
          exit(1);
      }

    loadEnd();
  }


    /* Print an error message if an error occured */
    if (codeFlag != valid) {
      loadError(loadText, loadLine, loadMessage);
      switch (codeFlag) {
        case invalidOpcode:
          break;

        case invalidForm:
          printCorrectForm(currInstr->Opcode);
          break;

        case invalidRegister:
          fprintf(stderr, "\n   VALID RegisterS RANGE FROM 0 to ");
          fprintf(stderr, "%d", registerCount);
          fprintf(stderr, "\n");
          break;

        case invalidLabel:
          fprintf(stderr, "\n   VALID LABELS RANGE FROM 0 to ");
          fprintf(stderr, "%d", labelCount);
          fprintf(stderr, "\n");
          break;

        case badLabel:
        case invalidString:
          break;

        default:
          perror(">>> INTERNAL ERROR IN uLOAD <<<");
          exit(1);
      }
    }

    /* Process debugging information and update counters */
    fillInformation(currInstr, loadText, loadLine);

    loadText[0] = '\0';
    loadLine++;
    codeLine++;

    if (codeLine >= codeSize) {
      fprintf(stderr, ">>> CODE MEMORY OVERFLOW <<<\n");
      exit(1);
    }
  } while (yytok != tEOF);


  /* Resolve labels if source was loaded successfully */
  if (loadFlag == valid) {
    for (i = 0; i < labelCount; i++) {
      lab = label[i];

      while ((lab < 0) && (lab != undefinedLabel)) {
        label[i] = label[-label[i]];
        lab = label[i];
      }
    }
  }

  return (loadFlag == valid) ?1 :0;
}


/*************************************************************************
 * processWhitespace
 *   Process whitespace and update debugging information.
 *************************************************************************/
uTOKEN processWhitespace(void)
{
  do {
    yylex();

    if ((yytok == tWhitespace) || (yytok == tComment)) {
      strcat(loadText, yytext);
    }
  } while ((yytok == tWhitespace) || (yytok == tComment));

  if ((yytok != tEOLN) && (yytok != tEOF)) {
    strcat(loadText, yytext);
  }

  return yytok;
}


/*************************************************************************
 * clearInstruction
 *   Clear (initialize) the instruction structure.
 *************************************************************************/
void clearInstruction(uINSTRUCTION *instr)
{
  instr->Text = (char *) NULL;
  instr->Line = -1;

  instr->Opcode = oEND;

  instr->Operand[0].Mode     = mNone;
  instr->Operand[0].Register = -1;
  setOperandValue(&(instr->Operand[0]), -1); 

  instr->Operand[1].Mode     = mNone;
  instr->Operand[1].Register = -1;
  setOperandValue(&(instr->Operand[1]), -1); 

  instr->Operand[2].Mode     = mNone;
  instr->Operand[2].Register = -1;
  setOperandValue(&(instr->Operand[2]), -1); 
}


/*************************************************************************
 * fillInformation
 *   Fill the instruction with debugging information.
 *************************************************************************/
void fillInformation(uINSTRUCTION *instr, char *text, int line)

{
  int len = strlen(text) + 1;

  instr->Text = (char *) calloc(len, sizeof(char));
  strcpy(instr->Text, text);

  instr->Line = line;
}


/*************************************************************************
 * loadOpcode
 *   Load (and identify) the next opcode.
 *************************************************************************/
void loadOpcode(uINSTRUCTION *instr)
{
  if (codeFlag == valid) {
    while (processWhitespace() == tEOLN) {
      loadText[0] = '\0';
      loadLine++;
    }

    switch (yytok) {
      case tHLT     : instr->Opcode = oHLT ;  break;

      case tRD      : instr->Opcode = oRD    ;  break;
      case tRDF     : instr->Opcode = oRDF   ;  break;
      case tRDS     : instr->Opcode = oRDS   ;  break;
      case tWRT     : instr->Opcode = oWRT   ;  break;
      case tWRTLN   : instr->Opcode = oWRTLN ;  break;
      case tWRTS    : instr->Opcode = oWRTS  ;  break;
      case tWRTLNS  : instr->Opcode = oWRTLNS;  break;
      case tMOV     : instr->Opcode = oMOV   ;  break;
      
      case tNEG     : instr->Opcode = oNEG   ;  break;
      case tADD     : instr->Opcode = oADD   ;  break;
      case tSUB     : instr->Opcode = oSUB   ;  break;
      case tMUL     : instr->Opcode = oMUL   ;  break;
      case tDIV     : instr->Opcode = oDIV   ;  break;
      case tMOD     : instr->Opcode = oMOD   ;  break;

      case tNEGF    : instr->Opcode = oNEGF ;  break;
      case tADDF    : instr->Opcode = oADDF ;  break;
      case tSUBF    : instr->Opcode = oSUBF ;  break;
      case tMULF    : instr->Opcode = oMULF ;  break;
      case tDIVF    : instr->Opcode = oDIVF ;  break;

      case tCASTI   : instr->Opcode = oCASTI; break;
      case tCASTF   : instr->Opcode = oCASTF; break;
      
      case tPUSH    : instr->Opcode = oPUSH;  break;
      case tPOP     : instr->Opcode = oPOP ;  break;
      case tNEGS    : instr->Opcode = oNEGS;  break;
      case tADDS    : instr->Opcode = oADDS;  break;
      case tSUBS    : instr->Opcode = oSUBS;  break;
      case tMULS    : instr->Opcode = oMULS;  break;
      case tDIVS    : instr->Opcode = oDIVS;  break;
      case tMODS    : instr->Opcode = oMODS;  break;

      case tNEGSF   : instr->Opcode = oNEGSF;  break;
      case tADDSF   : instr->Opcode = oADDSF;  break;
      case tSUBSF   : instr->Opcode = oSUBSF;  break;
      case tMULSF   : instr->Opcode = oMULSF;  break;
      case tDIVSF   : instr->Opcode = oDIVSF;  break;

      case tCASTSI  : instr->Opcode = oCASTSI; break;
      case tCASTSF  : instr->Opcode = oCASTSF; break;

      case tLAB     : instr->Opcode = oLAB ;  break;

      case tANDS    : instr->Opcode = oANDS;   break;
      case tORS     : instr->Opcode = oORS;    break;
      case tNOTS    : instr->Opcode = oNOTS;   break;
  
      case tCMPEQS  : instr->Opcode = oCMPEQS; break;
      case tCMPGES  : instr->Opcode = oCMPGES; break;
      case tCMPGTS  : instr->Opcode = oCMPGTS; break;
      case tCMPLES  : instr->Opcode = oCMPLES; break;
      case tCMPLTS  : instr->Opcode = oCMPLTS; break;
      case tCMPNES  : instr->Opcode = oCMPNES; break;

      case tCMPEQSF : instr->Opcode = oCMPEQSF; break;
      case tCMPGESF : instr->Opcode = oCMPGESF; break;
      case tCMPGTSF : instr->Opcode = oCMPGTSF; break;
      case tCMPLESF : instr->Opcode = oCMPLESF; break;
      case tCMPLTSF : instr->Opcode = oCMPLTSF; break;
      case tCMPNESF : instr->Opcode = oCMPNESF; break;

      case tBRTS    : instr->Opcode = oBRTS ;  break;
      case tBRFS    : instr->Opcode = oBRFS ;  break;

      case tBR      : instr->Opcode = oBR  ;  break;
      case tBEQ     : instr->Opcode = oBEQ ;  break;
      case tBGE     : instr->Opcode = oBGE ;  break;
      case tBGT     : instr->Opcode = oBGT ;  break;
      case tBLE     : instr->Opcode = oBLE ;  break;
      case tBLT     : instr->Opcode = oBLT ;  break;
      case tBNE     : instr->Opcode = oBNE ;  break;

      case tBEQF    : instr->Opcode = oBEQF ;  break;
      case tBGEF    : instr->Opcode = oBGEF ;  break;
      case tBGTF    : instr->Opcode = oBGTF ;  break;
      case tBLEF    : instr->Opcode = oBLEF ;  break;
      case tBLTF    : instr->Opcode = oBLTF ;  break;
      case tBNEF    : instr->Opcode = oBNEF ;  break;

      case tCALL    : instr->Opcode = oCALL;  break;
      case tRET     : instr->Opcode = oRET ;  break;
      
      case tPRTS    : instr->Opcode = oPRTS;  break;
      case tPRTR    : instr->Opcode = oPRTR;  break;

      case tEOF     : break;

      default:
        sprintf(loadMessage, "FOUND %s, EXPECTING OPCODE OR EOF", yytext);
        codeFlag = invalidOpcode;
    }
  }
}


/*************************************************************************
 * loadSource
 *   Load (and validate) a source operand.
 *************************************************************************/
void loadSource(uINSTRUCTION *instr, int opNum)

{
  int reg, val, result;
  float valF;
  size_t len;
 
  if (codeFlag == valid) {
    processWhitespace();
    switch (yytok) {
      case tImmediate:
        sscanf(yytext, "#%d", &val);
        
        instr->Operand[opNum].Mode  = mImmediate;
        setOperandValue(&(instr->Operand[opNum]), val);
        break;

      case tImmediateF:
        sscanf(yytext, "#%f", &valF); 
        
        instr->Operand[opNum].Mode = mImmediate;
        setOperandValue(&(instr->Operand[opNum]), valF);
        break;

      case tImmediateS:
        //Stip off #" from front and " from the back
        len = strlen(yytext);
        char stripped[len-2];
        
        strncpy(stripped, yytext + 2, len - 2);
        stripped[len - 3] = '\0';           //Null terminated & remove trailing "
        
        //Parse string, replacing escape sequences.
        char parsedString[strlen(stripped)];
        
        //char* parsedString;
        //parsedString = (char*)malloc(strlen(stripped) * sizeof(char));
        //if(parsedString == NULL){
        //  fprintf(stderr, "Malloc error: Out of memory\n");
        //  exit(EXIT_FAILURE);
        //}
        replaceEscapeSequences(stripped, parsedString);
        
        //Save parsed string into operand.
        instr->Operand[opNum].Mode = mImmediate;
        setOperandValue(&(instr->Operand[opNum]), parsedString);
        break;

      case tRegister:
        sscanf(yytext, "%*[Dd]%d", &reg);
        
        instr->Operand[opNum].Mode     = mRegister;
        instr->Operand[opNum].Register = reg;
        validateRegister(reg);
        break;

      case tIndexed:
        sscanf(yytext, "%d(%*[Dd]%d)", &val, &reg);
        
        instr->Operand[opNum].Mode     = mIndexed;
        instr->Operand[opNum].Register = reg;
        setOperandValue(&(instr->Operand[opNum]), val);
        validateRegister(reg);
        break;

      case tIndirect:
        sscanf(yytext, "@%d(%*[Dd]%d)", &val, &reg);
        
        instr->Operand[opNum].Mode     = mIndirect;
        instr->Operand[opNum].Register = reg;
        setOperandValue(&(instr->Operand[opNum]), val);
        validateRegister(reg);
        break;

      case tStackRegister:
        sscanf(yytext, "%*[Ss]%*[Pp]");

        instr->Operand[opNum].Mode     = mRegister;
        instr->Operand[opNum].Register = stackRegister;
        break;

      case tStackIndexed:
        sscanf(yytext, "%d(%*[Ss]%*[Pp])", &val);

        instr->Operand[opNum].Mode     = mIndexed;
        instr->Operand[opNum].Register = stackRegister;
        setOperandValue(&(instr->Operand[opNum]), val);
        break;

      case tStackIndirect:
        sscanf(yytext, "@%d(%*[Ss]%*[Pp])", &val);

        instr->Operand[opNum].Mode     = mIndirect;
        instr->Operand[opNum].Register = stackRegister;
        setOperandValue(&(instr->Operand[opNum]), val);
        break;

      case tLabel:
        sprintf(loadMessage, "%s CANNOT BE A SOURCE OPERAND", yytext);
        codeFlag = invalidForm;
        break;

      case tEOLN:
      case tEOF :
        strcpy(loadMessage, "MISSING SOURCE OPERAND");
        codeFlag = invalidForm;
        break;

      default:
        sprintf(loadMessage, "FOUND %s, EXPECTING SOURCE OPERAND", yytext);
        codeFlag = invalidForm;
    }
  }
}

/*************************************************************************
 * replaceEscapeSequences
 *   Iterate through a string looking for valid escape sequences, and
 *     replace them with their corresponding char ascii values.
 *   
 *   Valid escape sequences: \n, \r, \t, \v, \\. 
 *      All other escape sequences(starting with '\') are invalid.
 *
 *   Precondition:  String s is null terminated.
 *   Postcondition: String parsedString is null terminated.
 *************************************************************************/
static void replaceEscapeSequences(char* s, char* parsedString)
{
  enum state {
    NORMAL, ESCAPE
  };
  char ch;
  size_t size;
  int parsedIndex;
  enum state currentState;
  parsedIndex = 0;
  currentState = NORMAL;

  //Iterate through all characters including the null terminator.
  size = strlen(s);
  for(int i = 0; i <= size; i++)
  {
    ch = s[i];
    switch(currentState)
    {
      case NORMAL:
        if(ch == '\\'){
          currentState = ESCAPE;
        }
        else{
          currentState = NORMAL;
          parsedString[parsedIndex++] = ch;
        }
        break;
      case ESCAPE:
        if(ch == 'n'){
          currentState = NORMAL;
          parsedString[parsedIndex++] = '\n';
        } 
        else if(ch == 'r'){
          currentState = NORMAL;
          parsedString[parsedIndex++] = '\r';
        }
        else if(ch == 't'){
          currentState = NORMAL;
          parsedString[parsedIndex++] = '\t';
        }
        else if(ch == 'v'){
          currentState = NORMAL;
          parsedString[parsedIndex++] = '\v';
        }
        else if(ch == '\\'){
          currentState = NORMAL;
          parsedString[parsedIndex++] = '\\';
        }
        else if(ch == '\0'){
          parsedString[parsedIndex++] = '\0'; //Ensure always null terminated.
          sprintf(loadMessage, "STRING CAN'T END WITH ESCAPE CHARACTER");
          codeFlag = invalidString;
          return;
        }
        else{
          parsedString[parsedIndex++] = '\0'; //Ensure always null terminated.
          sprintf(loadMessage, "INVALID STRING ESCAPE SEQUENCE \\%c", ch);
          codeFlag = invalidString;
          return;
        }
        break;
    }
  }
}

/*************************************************************************
 * loadTarget
 *   Load (and validate) a target operand.
 *************************************************************************/
void loadTarget(uINSTRUCTION *instr, int opNum)
{
  int reg, val;

  if (codeFlag == valid) {
    processWhitespace();

    switch (yytok) {
      case tImmediate:
        sprintf(loadMessage, "%s CANNOT BE A DESTINATION OPERAND", yytext);
        codeFlag = invalidForm;
        break;
 
      case tImmediateF:
        sprintf(loadMessage, "%s CANNOT BE A DESTINATION OPERAND", yytext);
        codeFlag = invalidForm;
        break;

      case tImmediateS:
        sprintf(loadMessage, "%s CANNOT BE A DESTINATION OPERAND", yytext);
        codeFlag = invalidForm;
        break;
      
    case tRegister:
        sscanf(yytext, "%*[Dd]%d", &reg);

        instr->Operand[opNum].Mode     = mRegister;
        instr->Operand[opNum].Register = reg;
        validateRegister(reg);
        break;

      case tIndexed:
        sscanf(yytext, "%d(%*[Dd]%d)", &val, &reg);

        instr->Operand[opNum].Mode     = mIndexed;
        instr->Operand[opNum].Register = reg;
        setOperandValue(&(instr->Operand[opNum]), val);
        validateRegister(reg);
        break;

      case tIndirect:
        sscanf(yytext, "@%d(%*[Dd]%d)", &val, &reg);

        instr->Operand[opNum].Mode     = mIndirect;
        instr->Operand[opNum].Register = reg;
        setOperandValue(&(instr->Operand[opNum]), val);
        validateRegister(reg);
        break;

      case tStackRegister:
        sscanf(yytext, "%*[Ss]%*[Pp]");

        instr->Operand[opNum].Mode     = mRegister;
        instr->Operand[opNum].Register = stackRegister;
        break;

      case tStackIndexed:
        sscanf(yytext, "%d(%*[Ss]%*[Pp])", &val);

        instr->Operand[opNum].Mode     = mIndexed;
        instr->Operand[opNum].Register = stackRegister;
        setOperandValue(&(instr->Operand[opNum]), val);
        break;

      case tStackIndirect:
        sscanf(yytext, "@%d(%*[Ss]%*[Pp])", &val);

        instr->Operand[opNum].Mode     = mIndirect;
        instr->Operand[opNum].Register = stackRegister;
        setOperandValue(&(instr->Operand[opNum]), val);
        break;

      case tLabel:
        sprintf(loadMessage, "%s CANNOT BE A DESTINATION OPERAND", yytext);
        codeFlag = invalidForm;
        break;

      case tEOLN:
      case tEOF :
        strcpy(loadMessage, "MISSING DESTINATION OPERAND");
        codeFlag = invalidForm;
        break;

      default:
        sprintf(loadMessage, "FOUND %s, EXPECTING DESTINATION OPERAND", yytext);
        codeFlag = invalidForm;
    }
  }
}


/*************************************************************************
 * loadLabel
 *   Load (and validate) a label operand.
 *************************************************************************/
void loadLabel(uINSTRUCTION *instr, int opNum)

{
  int lab;

  if (codeFlag == valid) {
    processWhitespace();

    switch (yytok) {
      case tLabel:
        sscanf(yytext, "%*[Ll]%d", &lab);

        instr->Operand[opNum].Mode  = mLabel;
        setOperandValue(&(instr->Operand[opNum]), lab);
        validateLabel(lab);
        break;

      case tEOLN:
      case tEOF :
        strcpy(loadMessage, "MISSING LABEL OPERAND");
        codeFlag = invalidForm;
        break;

      default:
        sprintf(loadMessage, "FOUND %s, EXPECTING LABEL OPERAND", yytext);
        codeFlag = invalidForm;
    }
  }
}


/*************************************************************************
 * loadEnd
 *   Check that nothing else is on the instruction line.
 *************************************************************************/
void loadEnd(void)
{
  if (codeFlag == valid) {
    processWhitespace();

    switch (yytok) {
      case tEOLN:
      case tEOF :
        break;

      default:
        sprintf(loadMessage, "FOUND %s, EXPECTING EOLN OR EOF", yytext);
        codeFlag = invalidForm;
    }
  }
}


/*************************************************************************
 * validateRegister
 *   Check the validity (range) of a register operand.
 *************************************************************************/
void validateRegister(int reg)
{
  if (codeFlag == valid) {
    if ((reg < 0 ) || (reg >= registerCount)) {
      sprintf(loadMessage, "D%d DOES NOT EXIST", reg);
      codeFlag = invalidRegister;
    }
  }
}


/*************************************************************************
 * validateLabel
 *   Check the validity (range) of a label operand.
 *************************************************************************/
void validateLabel(int lab)
{
  if (codeFlag == valid) {
    if ((lab < 0) || (lab >= labelCount)) {
      sprintf(loadMessage, "L%d IS NOT VALID", lab);
      codeFlag = invalidLabel;
    }
  }
}


/*************************************************************************
 * loadError
 *   Report an error during program loading.
 *************************************************************************/
void loadError(char *text, int line, char *mesg)
{
  loadFlag = invalid;
  
  /* Consume the remainder of the source line */
  while ((yytok != tEOLN) && (yytok != tEOF)) {
    yylex();

  /*Check if not only invalid opcode, but also
    invalid character found(uscan.l)*/
  if(yytok == tError) 
    break;

    if ((yytok != tEOLN) && (yytok != tEOF)) {
      strcat(text, yytext);
    }
  }

  /* Print an error message */
  fprintf(stderr, ">>>ERROR DURING LOAD: %s\n", mesg);
  fprintf(stderr, "   LINE %d: %s\n", line, text);
  fprintf(stderr, "\n");
}


/*************************************************************************
 * printCorrectForm 
 *   Prints the correct form of the expected opcode operand pairing.
 *************************************************************************/
void printCorrectForm(uOPCODE opcode)
{
  fprintf(stderr, "\n   CORRECT FORM:  ");

  switch (opcode) {
    case oHLT :  fprintf(stderr, "HLT"                    );  break;

    case oRD     :  fprintf(stderr, "RD  dst"             );  break;
    case oRDF    :  fprintf(stderr, "RDF dst"             );  break;
    case oRDS    :  fprintf(stderr, "RDS dst"             );  break;
    case oWRT    :  fprintf(stderr, "WRT  src"            );  break;
    case oWRTS   :  fprintf(stderr, "WRTS"                );  break;
    case oWRTLN  :  fprintf(stderr, "WRTLN  src"          );  break;
    case oWRTLNS :  fprintf(stderr, "WRTLNS"              );  break;

    case oMOV :  fprintf(stderr, "MOV  src  dst"       );  break;
    case oNEG :  fprintf(stderr, "NEG  src  dst"       );  break;
    case oADD :  fprintf(stderr, "ADD  src1  src2  dst");  break;
    case oSUB :  fprintf(stderr, "SUB  src1  src2  dst");  break;
    case oMUL :  fprintf(stderr, "MUL  src1  src2  dst");  break;
    case oDIV :  fprintf(stderr, "DIV  src1  src2  dst");  break;
    case oMOD :  fprintf(stderr, "MOD  src1  src2  dst");  break;

    case oNEGF :  fprintf(stderr, "NEGF  src  dst"       );  break;
    case oADDF :  fprintf(stderr, "ADDF  src1  src2  dst");  break;
    case oSUBF :  fprintf(stderr, "SUBF  src1  src2  dst");  break;
    case oMULF :  fprintf(stderr, "MULF  src1  src2  dst");  break;
    case oDIVF :  fprintf(stderr, "DIVF  src1  src2  dst");  break;
    
    case oCASTI: fprintf(stderr, "CASTI  src  dst"     );  break;
    case oCASTF: fprintf(stderr, "CASTF  src  dst"     );  break;
  
    case oPUSH:  fprintf(stderr, "PUSH  src"           );  break;
    case oPOP :  fprintf(stderr, "POP  targ"           );  break;
                                                       
    case oNEGS:  fprintf(stderr, "NEGS"                );  break;
    case oADDS:  fprintf(stderr, "ADDS"                );  break;
    case oSUBS:  fprintf(stderr, "SUBS"                );  break;
    case oMULS:  fprintf(stderr, "MULS"                );  break;
    case oDIVS:  fprintf(stderr, "DIVS"                );  break;
    case oMODS:  fprintf(stderr, "MODS"                );  break;
                                                    
    case oNEGSF:  fprintf(stderr, "NEGSF"                );  break;
    case oADDSF:  fprintf(stderr, "ADDSF"                );  break;
    case oSUBSF:  fprintf(stderr, "SUBSF"                );  break;
    case oMULSF:  fprintf(stderr, "MULSF"                );  break;
    case oDIVSF:  fprintf(stderr, "DIVSF"                );  break;
    
    case oCASTSI: fprintf(stderr, "CASTSI"         );  break;
    case oCASTSF: fprintf(stderr, "CASTSF"         );  break;
  
    case oLAB :  fprintf(stderr, "Ln:"                 );  break;
    
    case oANDS:  fprintf(stderr, "ANDS"                );  break;
    case oORS:   fprintf(stderr, "ORS"                 );  break;
    case oNOTS:  fprintf(stderr, "NOTS"                );  break;

    case oCMPEQS : fprintf(stderr, "CMPEQS"        );  break;
    case oCMPGES : fprintf(stderr, "CMPGES"        );  break;
    case oCMPGTS : fprintf(stderr, "CMPGTS"        );  break;
    case oCMPLES : fprintf(stderr, "CMPLES"        );  break;
    case oCMPLTS : fprintf(stderr, "CMPLTS"        );  break;
    case oCMPNES : fprintf(stderr, "CMPNES"        );  break;
    
    case oCMPEQSF : fprintf(stderr, "CMPEQSF"        );  break;
    case oCMPGESF : fprintf(stderr, "CMPGESF"        );  break;
    case oCMPGTSF : fprintf(stderr, "CMPGTSF"        );  break;
    case oCMPLESF : fprintf(stderr, "CMPLESF"        );  break;
    case oCMPLTSF : fprintf(stderr, "CMPLTSF"        );  break;
    case oCMPNESF : fprintf(stderr, "CMPNESF"        );  break;
                                                  
    case oBRTS : fprintf(stderr, "BRTS "         );  break;
    case oBRFS : fprintf(stderr, "BRFS "         );  break;
    
    case oBR  :  fprintf(stderr, "BR  lab"             );  break;
    case oBEQ :  fprintf(stderr, "BEQ  src1  src2  lab");  break;
    case oBGE :  fprintf(stderr, "BGE  src1  src2  lab");  break;
    case oBGT :  fprintf(stderr, "BGT  src1  src2  lab");  break;
    case oBLE :  fprintf(stderr, "BLE  src1  src2  lab");  break;
    case oBLT :  fprintf(stderr, "BLT  src1  src2  lab");  break;
    case oBNE :  fprintf(stderr, "BNE  src1  src2  lab");  break;
    
    case oBEQF :  fprintf(stderr, "BEQF  src1  src2  lab");  break;
    case oBGEF :  fprintf(stderr, "BGEF  src1  src2  lab");  break;
    case oBGTF :  fprintf(stderr, "BGTF  src1  src2  lab");  break;
    case oBLEF :  fprintf(stderr, "BLEF  src1  src2  lab");  break;
    case oBLTF :  fprintf(stderr, "BLTF  src1  src2  lab");  break;
    case oBNEF :  fprintf(stderr, "BNEF  src1  src2  lab");  break;

    case oCALL:  fprintf(stderr, "CALL  lab"           );  break;
    case oRET :  fprintf(stderr, "RET"                 );  break;
    
    case oPRTS:  fprintf(stderr, "PRTS"                );  break;
    case oPRTR:  fprintf(stderr, "PRTR"                );  break;

    default:
      perror(">>> INTERNAL ERROR IN CODE_FORM <<<");
      exit(1);
  }

  fprintf(stderr, "\n");
}
