/******************************************************************************
 *                                                                            *
 *   lib.c                                                                    *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <assert.h>
#include <stdio.h>
#include <limits.h>
#include "ioChar.h"
#include "types.h"
#include "lib.h"
#include "pmem.h"

/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/


/*****************************************************************************
 *                                                                           *
 *                           internal variables                              *
 *                                                                           *
 *****************************************************************************/
int uniqueInt = 0;

/***************************************
 *                                     *
 *        getUniqueInt                 *
 *                                     *
 ***************************************/
unsigned long int
getUniqueInt(void) {

  uniqueInt++;
  if (uniqueInt == (ULONG_MAX * 2) + 1)
    printMsg(FATAL_MSG, "getUniqueInt(): uniqueInt exceeded system capacity.");
  return uniqueInt;

}


/*********************************
 *                               *
 *    makeNewRsrvdVar            *
 *                               *
 *********************************/
char *
makeNewRsrvdVar(MEMORY heapDesc) {

  unsigned long int     varLen=2,
                        resLen,
                        newVarCntr,
                        varCntr = getUniqueInt();
  char   *varStr;

  newVarCntr = varCntr;

  while (varCntr / 10) {
    varLen++;
    varCntr = varCntr / 10;
  }
  resLen = strlen(RES_PREFIX); 
  varLen = varLen + resLen;
  varStr = (char *)MemHeapAlloc(heapDesc, varLen, sizeof(char));

  strcpy(varStr,RES_PREFIX);
  sprintf(&varStr[resLen],"%d",newVarCntr);

  return(varStr);

}


/***************************************
 *                                     *
 *        libStrdup                    *
 *                                     *
 ***************************************/
char
*libStrdup(MEMORY heapDesc, char *str)
{
	int len      = 0;
	char *result = NULL;

	assert(str);

	len = strlen(str);
	result = (char *) MemHeapAlloc(heapDesc, len + 1, sizeof(char));
	assert(result);

	strcpy(result, str);
	return(result);
}	


/*********************************
 *                               *
 *    libStrListdup              *
 *                               *
 *********************************/
STR_LIST *
libStrListdup(MEMORY HD, STR_LIST *strList) {

  STR_LIST *newList;
  char *str;

  if (strList) {
    str = libStrdup(HD, StrListHead(strList));
    newList = StrListCons(str, libStrListdup(HD, StrListTail(strList)), HD);
  }
  else
    newList = NULL;

  return newList;
  
}


/**************************
 *                        *
 *    PtrArrayLen         *
 *                        *
 **************************/
int
PtrArrayLen(char **array)
{
/* Only finds the find on Null terminated arrays,
   with each item in the array a pointer */
     int count = 0;
     while (array[count])
	  count++;
     return(count);
}


/**************************
 *                        *
 *    lb_BuildCombString  *
 *                        *
 **************************/
char *
lb_BuildCombString(MEMORY hd, char *s1, char *s2) {

  char *result;
  int   resLen = strlen(RES_PREFIX)  + strlen(s1) + 1 + strlen(s2) + 1;

  result = (char *)MemHeapAlloc(hd, resLen, sizeof(char));

  strcpy(result, RES_PREFIX);
  strcat(result, s1);
  strcat(result, "_");
  strcat(result, s2);

  return(result);

}


/**************************
 *                        *
 *    lb_BuildMacroName   *
 *                        *
 **************************/
char *
lb_BuildMacroName(MEMORY hd, char *funName, char *macroName) {

  char *result;
  int   resLen = strlen(RES_PREFIX) +1+ strlen(funName) +1+ strlen(macroName);

  result = (char *)MemHeapAlloc(hd, resLen, sizeof(char));

  strcpy(result, RES_PREFIX);
  strcat(result, "_");
  strcat(result, funName);
  strcat(result, "_");
  strcat(result, macroName);

  return(result);

}
