/******************************************************************************
 *                                                                            *
 *   typecheck.h                                                              *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#ifndef __TYPECHECK_H__
#define __TYPECHECK_H__

#include "codetab.h"
#include "symtab.h"
#include "pmem.h"
#include "ioChar.h"
#include "types.h"
#include "lib.h"
#include "list.h"
#include "parse.h"

/**************************************************/
/*  the only functions available to the outside:  */
/**************************************************/

extern void     tc_open_typechecker(void);
extern void     tc_close_typechecker(int kill_a_def);
extern ST_TYPE *tc_typecheck_PE_EXPR(PE_EXPR *expr);
extern void     tc_typecheck_PE_DEF(PE_DEF *def, ST_KEY fnkey);

#endif
