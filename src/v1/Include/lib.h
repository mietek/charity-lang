/******************************************************************************
 *                                                                            *
 *   lib.h                                                                    *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __LIB_H__
#define __LIB_H__

#include "pmem.h"
#include "list.h"

extern char             *libStrdup(MEMORY heapDesc, char *s);
extern STR_LIST         *libStrListdup(MEMORY heapDesc, STR_LIST *strList);
extern int               PtrArrayLen(char **array);
extern unsigned long int getUniqueInt(void);
extern char             *makeNewRsrvdVar(MEMORY heapDesc);
extern char             *lb_BuildCombString(MEMORY hd, char *combKind, char *typeName);
extern char             *lb_BuildMacroName(MEMORY hd, char *funName, char *macroName);


#endif
