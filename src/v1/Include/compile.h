/******************************************************************************
 *                                                                            *
 *   compile.h                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __COMPILER__ 
#define __COMPILER__ 

#include "machine.h"
#include "symtab.h"
 
struct _COMB_EXPR;

typedef struct _CLOSURE { 
  struct _V_INSTR *rec; 
  struct _V_INSTR *clo; 
  ST_TYPE *type;
} CLOSURE; 

typedef int CLO_TAB_ENTRY;

typedef struct _MACRO_CODE { 
  M_INSTR *code; 
} MACRO_CODE; 

extern void        CompilerConstruct(void); 
extern void        CompilerReset(void); 
extern void        CompilerDestruct(void); 
 
extern M_INSTR    *Compile(char *functionName, struct _COMB_EXPR *code);
extern M_INSTR    *_CompileHalt(struct _COMB_EXPR *code);

#endif
