/******************************************************************************
 *                                                                            *
 *   pm.h                                                                     *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __PM_H__
#define __PM_H__

#include "coreTL.h"
#include "parse.h"
#include "pmem.h"

/**************************************************************************
 *                                                                        *
 *           Global variables                                             *
 *                                                                        *
 **************************************************************************/
extern CT_EXPR            *PmResult;    /* end product of pm translation */
extern MEMORY              ctHD;  /* heap descriptor for core expr. */






/**************************************************************************
 *                                                                        *
 *           PROTOTYPES (EXTERNAL ONLY)                                   *
 *                                                                        *
 *   All external functions should be prefixed with pm
 *                                                                        *
 **************************************************************************/
extern void                pmInit(void);
extern void                pmDestructCTHeap(void);
extern CT_EXPR            *pmTranslate(PE_EXPR  *peExpr, int kill_a_def);
extern BBOOL               ct_isVarBase(PE_PATT *patt);
extern CT_VAR_BASE        *ctMakeVarBase(MEMORY heap,char *var, BBOOL copyVar);

extern CT_TERM            *ctMakeStructTerm(MEMORY heap, char *structName,
                                            BBOOL copyStruct);
extern CT_EXPR            *ctMakeVarExpr(MEMORY heap,char *var, BBOOL copyVar);
extern CT_EXPR            *ctMakeBangExpr(MEMORY heap);
extern CT_EXPR            *ctMakeAPPExpr(MEMORY heap,
                                         CT_TERM *term, CT_EXPR *expr);
extern CT_EXPR            *ctMakePairExpr(MEMORY heap,
                                         CT_EXPR *exprL, CT_EXPR *exprR);
extern CT_TERM            *ctMakeAbsTerm(MEMORY heap, CT_VAR_BASE *vb,
                                         CT_EXPR *rhs);
extern CT_TERM            *ctMakeFunTerm(MEMORY heap, char *funName,
                                         CT_PHRASE **macros,BBOOL copyFunName);

extern CT_LIST_EXPRADDR *ctExprExpr(MEMORY heap,CT_EXPR **expr1,
                                    CT_EXPR *expr2);
extern CT_LIST_EXPRADDR *ctTermExpr(MEMORY heap, CT_TERM *term, CT_EXPR *expr);
extern BBOOL               ctExprCmp(CT_EXPR *expr1, CT_EXPR *expr2);


#endif
