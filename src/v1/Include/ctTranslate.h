/******************************************************************************
 *                                                                            *
 *   ctTranslate.h                                                            *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __CTTRANSLATE_H__
#define __CTTRANSLATE_H__

#include "coreTL.h"
#include "codetab.h"
#include "pmem.h"
#include "parse.h"

/*********************************
 *                               *
 *    global data                *
 *                               *
 *********************************/

extern CT_VAR_BASE          ct_vb_env;
extern BBOOL                printCT_EXPR;

/*********************************
 *                               *
 *    translation routines       *
 *                               *
 *********************************/
extern void       ctTranslateConstruct(void);
extern void       ctTranslateDestruct(void);
extern void       ctTranslateReset(void);

extern void       ct_TranslateOpen(void);
extern void       ct_TranslateClose(void);

extern COMB_EXPR *ctTranslate(char *funName, CT_VAR_BASE *var_base, CT_EXPR *expr);

extern CT_EXPR *ctPreTranslate (CT_EXPR *expr);     /* [#@] */

/*********************************
 *                               *
 *    Build new comb expr'ns     *
 *                               *
 *********************************/

extern COMB_EXPR *CombExprConstructor(MEMORY heapDesc, char *constr);
extern COMB_EXPR *CombExprDestructor(MEMORY heapDesc, char *destr);

extern COMB_EXPR *CombExprComposition(MEMORY heapDesc, 
				      COMB_EXPR *l, COMB_EXPR *r);

/* [H-O] ALTERED THE TYPE OF THE LAST PARAMETER (SEE codetab.h): */

extern COMB_EXPR *CombExprNew (MEMORY       heapDesc,
			       COMB_CLASS   class,
			       char        *name,
			       int          numParms,
			       COMB_PHR   **parms);

extern COMB_EXPR *CombExprPair(MEMORY heapDesc, 
			       COMB_EXPR *l, COMB_EXPR *r);
extern COMB_EXPR *CombExprVar(MEMORY heapDesc, 
			      CT_VAR_BASE *var_base, CT_EXPR *var_expr);
extern COMB_EXPR *CombExprP0(MEMORY heapDesc);
extern COMB_EXPR *CombExprP1(MEMORY heapDesc);

extern COMB_EXPR *CombExprDuplicate(MEMORY heapDesc, COMB_EXPR *expr);

/* [BI] ADDED THESE PROTOTYPES (SEE ctTranslate.c): */

extern COMB_EXPR *CombInt  (MEMORY heapDesc,
			    int    i);
extern COMB_EXPR *CombChar (MEMORY heapDesc,
			    char   c);


/*********************************
 *                               *
 *    mapEi                      *
 *                               *
 *********************************/

/* [H-O] ALTERED THE TYPE OF THE THIRD PARAMETER (SEE compile.c): */

extern COMB_EXPR *mapEi (MEMORY      heapDesc,
			 char       *structor,
			 COMB_PHR  **parametric,
			 COMB_EXPR  *state);

/* [H-O] ADDED THIS FUNCTION (SEE compile.c): */

extern COMB_EXPR *mapEiHO (MEMORY     heapDesc,
			   char      *structor,
			   COMB_PHR **parametric);


/*********************************
 *                               *
 *    display                    *
 *                               *
 *********************************/

extern void         CombExprPrint(COMB_EXPR *expr);

extern int          _showCT_expr(CT_EXPR *expr, int indent);
extern int          _showCT_VarBase(CT_VAR_BASE *var_base, BBOOL spaceBeforeVar);
extern void         ctShowVarBase(CT_VAR_BASE *var_base);

extern CT_VAR_BASE *VarBasePairNew(MEMORY hd, CT_VAR_BASE *l, CT_VAR_BASE *r);

#endif
