/*************************************************************************
 * ucode.c
 *   Driver for the uMachine emulator.
 *************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "umach.h"
#include "uload.h"
#include "uexec.h"
#include "uconfig.h"

/* External interface to FLEX */
extern FILE *yyin;

/* External interface to the uMachine */
extern uINSTRUCTION uCode [codeSize];
extern int          uLabel[labelCount];

/* Main routine */
int main(int argc, char *argv[])
{
  /* Check command line parameters */
  if (argc != 2) {
    fprintf(stderr, "correct usage: %s <filename>\n", argv[0]);
    return 1;
  }

  processConfigFile();
  
  /* Open the source file (attaching to external yyin) */
  if ((yyin = fopen(argv[1], "r")) == (FILE *) NULL) {
    fprintf(stderr, ">>>ERROR - CANNOT OPEN SOURCE FILE\n");
    fprintf(stderr, "     CHECK THAT %s EXISTS\n", argv[1]);
    return 2;
  }
  
  /* Attempt to load and execute the program */
  fprintf(stdout, "     uMachine  Interpreter     \n");
  fprintf(stdout, " (-*-)  [VERSION 2.0DV]  (-*-) \n");
  fprintf(stdout, "-------------------------------\n");

  uReset();

  if (uLoad(uCode, uLabel)) {
    uExecute(uCode, uLabel);
  }
  
  fprintf(stdout, "-------------------------------\n");
  fprintf(stdout, " (-*-)  [VERSION 2.0DV]  (-*-) \n");
  fprintf(stdout, "     uMachine  Interpreter     \n");

  return 0;
}
