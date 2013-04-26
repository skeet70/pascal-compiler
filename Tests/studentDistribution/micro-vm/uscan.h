/*************************************************************************
 * uscan.h
 *   Define the tokens scanned from a uCode source file.
 *************************************************************************/
#ifndef uSCAN_H
#define uSCAN_H

typedef enum
{
  /* Special tokens */
  tWhitespace, tComment, tEOLN,

  /* Operand tokens */
  tImmediate, tImmediateF, tImmediateS,
  tRegister,      tIndexed,       tIndirect,
  tStackRegister, tStackIndexed,  tStackIndirect,
  tLabel,

  /* Opcode tokens */
  tHLT ,
  tRD  , tRDF, tRDS, tWRT, tWRTS, tWRTLN, tWRTLNS,
  tMOV , 
  tNEG ,tADD , tSUB , tMUL , tDIV , tMOD,
  tNEGF ,tADDF , tSUBF , tMULF , tDIVF,
  tCASTI, tCASTF,
  tPUSH, tPOP ,
  tNEGS, tADDS, tSUBS, tMULS, tDIVS, tMODS,
  tNEGSF, tADDSF, tSUBSF, tMULSF, tDIVSF,
  tCASTSI, tCASTSF,
  tLAB ,
  tANDS, tORS, tNOTS,
  tCMPEQS, tCMPGES, tCMPGTS, tCMPLES, tCMPLTS, tCMPNES,
  tCMPEQSF, tCMPGESF, tCMPGTSF, tCMPLESF, tCMPLTSF, tCMPNESF,
  tBRTS, tBRFS,
  tBR  ,
  tBEQ , tBGE , tBGT , tBLE , tBLT , tBNE ,
  tBEQF , tBGEF , tBGTF , tBLEF , tBLTF , tBNEF ,
  tCALL, tRET ,
  tPRTS, tPRTR,
  /* End-of-file token */
  tEOF,

  /* Error token */
  tError
} uTOKEN;

#endif /* uSCAN_H */
