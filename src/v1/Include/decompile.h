/******************************************************************************
 *                                                                            *
 *   decompile.h                                                              *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __DECOMPILE__
#define __DECOMPILE__

#include "codetab.h"
#include "machine_private.h"

extern void       deCompileConstruct(void);
extern void       deCompileDestruct(void);
extern void       deCompileReset(void);

extern COMB_EXPR *deCompile(V_INSTR *value, ST_TYPE *type);

extern BBOOL      isInRightDisplay(void);

/* closures for records */
extern CLOSURE   *getClosure(CLO_TAB_ENTRY clo_entry);
extern int        getNumClosures(void);

#endif
