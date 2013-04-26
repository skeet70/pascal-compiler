#include <stdio.h>
#include <string.h>
#include "uconfig.h"

const char* filename = "vm.config";
//DEFAULT VALUES
int READ_ANNOTATION=0;

int processConfigFile()
{
  FILE *pFile;
  pFile = fopen(filename, "r");

  char line[256];
  int linenum = 0;
  if(pFile == NULL)
  {
    printf("No vm.config file included. Using defaults defined in uconfig.c\n");
  }
  else
  {
    while(fgets(line, 256, pFile) != NULL)
    {
        char key[256];
		int value;

        linenum++;
        if(line[0] == '#') continue;  //Comment, ignore
		if(line[0] == '\n') continue; //Blank line, ignore
        if(sscanf(line, "%s %d", key, &value) != 2)
        {
                fprintf(stderr, "Syntax error, line %d\n", linenum);
                continue;
        }
	    //printf("Line %d:  Key: %s Value %d\n", linenum, key, value);
	    keyValueStateMachine(key, value);
    }
  }
  return 1;
}
/*************************************************************************
 * State Machine which sets the config variable that corresponds 
 *   to the given key with the given value.
 *************************************************************************/
static void keyValueStateMachine(char *key, int value)
{
  if(!strcmp(key, "READ_ANNOTATION"))
   READ_ANNOTATION = value;
  else
   printf("Invalid key\n");
}
