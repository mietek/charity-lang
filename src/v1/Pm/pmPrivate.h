/******************************************************************************
 *                                                                            *
 *   pmPrivate.h                                                              *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#include "list.h"
#include "parse.h"

#define SCRATCH_HEAP_SIZE 6000    /* heap used during pm translation */

/**************************************************************************
 *                                                                        *
 *           Datatypes & Structures                                       *
 *                                                                        *
 **************************************************************************/

typedef struct _PM_PHRASE {
  PE_LIST_PATT    *patts;
  CT_EXPR         *rhs;
}  PM_PHRASE;


/**************************************************************************
 *                                                                        *
 *           Global variables                                             *
 *                                                                        *
 **************************************************************************/
extern MEMORY       scratchHD;
extern MEMORY       ctHD;

extern CT_EXPR     *bangExpr;
extern CT_VAR_BASE *bangVarBase;

extern int          pmkill_a_def;

/**************************************************************************
 *                                                                        *
 *           PROTOTYPES (EXTERNAL ONLY)                                   *
 *                                                                        *
 **************************************************************************/

/* FROM transExprs.c */
extern CT_EXPR   *transPeExpr(PE_EXPR *peExpr);



/* FROM transPhrases.c */
extern CT_EXPR     *pmTransPhrs(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs);


/*  FROM pmLib.c  */
extern void            cleanup(void);
extern BBOOL           possiblyIncomplete(CT_EXPR *expr);

extern CT_VAR_BASE     *makeCoreVarBase(char *var);
extern CT_EXPR         *makeCoreVarExpr(MEMORY heap, char *var, BBOOL copyVar);

extern CT_LIST_EXPR       *CTExprListCons(CT_EXPR *x, CT_LIST_EXPR *list);
extern CT_EXPR            *CTExprListHead(CT_LIST_EXPR *list);
extern CT_LIST_EXPR       *CTExprListTail(CT_LIST_EXPR *list);
extern CT_LIST_EXPR       *CTExprListAppend(CT_LIST_EXPR *l1,CT_LIST_EXPR *l2);
extern int                 CTExprListLen(CT_LIST_EXPR *list);

extern CT_LIST_EXPRADDR *CTExprAddrListCons(CT_EXPR **x, CT_LIST_EXPRADDR *list);
extern CT_EXPR         **CTExprAddrListHead(CT_LIST_EXPRADDR *list);
extern CT_LIST_EXPRADDR *CTExprAddrListTail(CT_LIST_EXPRADDR *list);
extern int               CTExprAddrListLen(CT_LIST_EXPRADDR *list);
extern CT_LIST_EXPRADDR *CTExprAddrListAppend(CT_LIST_EXPRADDR *l1, CT_LIST_EXPRADDR *l2);

extern PM_LIST_PHRASE     *PMPhraseListCons(PM_PHRASE *x,PM_LIST_PHRASE *list);
extern PM_PHRASE          *PMPhraseListHead(PM_LIST_PHRASE *list);
extern PM_LIST_PHRASE     *PMPhraseListTail(PM_LIST_PHRASE *list);
extern int                 PMPhraseListLen(PM_LIST_PHRASE *list);

extern PE_LIST_PATT         *PE_PattListCons(PE_PATT *x, PE_LIST_PATT *l);
extern PE_PATT              *PE_PattListHead(PE_LIST_PATT *list);
extern PE_LIST_PATT         *PE_PattListTail(PE_LIST_PATT *list);
extern PE_LIST_PATT      *PE_PattListAppend(PE_LIST_PATT *l1,PE_LIST_PATT *l2);
extern PE_PATT              *PE_ListPattGetNthItem(int n, PE_LIST_PATT *patts);

extern INT_LIST             *Int_ListCons(int x, INT_LIST *l);
extern int                   Int_ListHead(INT_LIST *list);
extern INT_LIST             *Int_ListTail(INT_LIST *list);

extern VOIDPTR_LIST      *VoidPtrListCons(void *x, INT_LIST *l);
extern void              *VoidPtrListHead(VOIDPTR_LIST *list);
extern VOIDPTR_LIST      *VoidPtrListTail(VOIDPTR_LIST *list);
