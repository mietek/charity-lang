/******************************************************************************
 *                                                                            *
 *   emit.h                                                                   *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __EMIT__
#define __EMIT__

#include "machine.h"
typedef int  EMIT;

extern M_INSTR emitCode;

extern EMIT      EmitIndex(void);
extern void      EmitSet(EMIT index);
extern M_INSTR **EmitParm(int num);
extern void      EmitReset(void);
extern void      EmitConstruct(void);
extern void      EmitDestruct(void);
extern M_INSTR  *EmitNew(char *function);
extern void      EmitMsg(char *msg);
extern M_INSTR  *EmitAddr(void);
extern void      emit(void);
extern void      EmitShowCode(void);
extern void      EmitChk(void);
extern M_INSTR  *EmitStart(void);

#endif
