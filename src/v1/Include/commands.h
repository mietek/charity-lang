/******************************************************************************
 *                                                                            *
 *   commands.h                                                               *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#ifndef __COMMANDS_H__
#define __COMMANDS_H__

#include "parse.h"

int  ProcessCmd(PARSE_RESULT *result);
void Readfile(char *file);
extern STR_LIST           *ge_StrListCons(char *x, STR_LIST *l);
extern STR_LIST *g_strList_IncludeDirs;
extern void  initIncludeDirs(void);
extern void  ge_DestructIncDirs(void);
extern MEMORY incDirHD;

#endif
