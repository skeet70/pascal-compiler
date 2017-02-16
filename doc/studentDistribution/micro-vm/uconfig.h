/*************************************************************************
 * uconfig.h
 *   Micro machine configuation variables.
 *************************************************************************/
#ifndef uCONFIG_H
#define uCONFIG_H

extern int READ_ANNOTATION;

int processConfigFile(void);
static void keyValueStateMachine(char* key, int value);

#endif /* uCONFIG_H */

