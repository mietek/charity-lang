/******************************************************************************
 *                                                                            *
 *   pmLib.c                                                                  *
 *                                                                            *
 *   COPYRIGHT (c) 1995, 1996 by Charity Development Group.                   *
 *   All rights reserved.                                                     *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

/**************************************************************************
 *                                                                        *
 *           LIBRARY CODE FOR PATTERN MATCHING TRANSLATION                *
 *                                                                        *
 **************************************************************************/

#include <assert.h>
#include "ioChar.h"
#include "lib.h"
#include "list.h"
#include "pm.h"
#include "pmPrivate.h"


/**************************************************************************
 *                                                                        *
 *           Global variables                                             *
 *                                                                        *
 **************************************************************************/


/**************************************************************************
 *                                                                        *
 *           Prototypes (internal only)                                   *
 *                                                                        *
 **************************************************************************/


/**************************************************************************
 *                                                                        *
 *           Function Definitions                                         *
 *                                                                        *
 **************************************************************************/

/*********************************
 *                               *
 *    cleanup                    *
 *                               *
 *********************************/
void
cleanup(void) {

  MemDealloc(scratchHD);

}



/*********************************
 *                               *
 *    possiblyIncomplete         *
 *                               *
 *********************************/
BBOOL
possiblyIncomplete(CT_EXPR *expr) {

  if ( (expr->tag == CT_APP) && 
       ((expr->info.app.term->tag == CT_T_CASE) || 
        (expr->info.app.term->tag == CT_T_ABS)) )
      return BTRUE;
  else
      return BFALSE;

}   /*  end possiblyIncomplete()  */


/*********************************
 *                               *
 *    ctMakeVarBase              *
 *                               *
 *********************************/
CT_VAR_BASE *
ctMakeVarBase(MEMORY heap, char *var, BBOOL copyVar) {

    /* if var is NULL then create () for a variable base */

    CT_VAR_BASE *vbase = (CT_VAR_BASE *)MHA(heap, 1, sizeof(CT_VAR_BASE));

    if ( var ) {
        vbase->tag = CT_VB_VAR;
        if ( copyVar == BTRUE ) 
            vbase->info.var = libStrdup(heap, var);
        else
            vbase->info.var = var;
    }   /*  fi  */
    else
        vbase->tag = CT_VB_BANG;

  return(vbase);

}


/*********************************
 *                               *
 *    ctMakeStructTerm           *
 *                               *
 *********************************/
CT_TERM *
ctMakeStructTerm(MEMORY heap, char *structName, BBOOL copyVar){

  CT_TERM *result = (CT_TERM *)MHA(heap, 1, sizeof(CT_TERM));
  result->tag = CT_T_STRUCTOR;

  if ( copyVar == BTRUE )
      result->info.struct_name = libStrdup(heap, structName);
  else
      result->info.struct_name = structName;

  return result;

}   /*  end ctMakeStructTerm  */


/*********************************
 *                               *
 *    ctMakeFunTerm              *
 *                               *
 *********************************/
CT_TERM *
ctMakeFunTerm(MEMORY heap, char *funName, 
              CT_PHRASE **macros, BBOOL copyFunName){

  CT_TERM *result = (CT_TERM *)MHA(heap, 1, sizeof(CT_TERM));
  CT_FUNCTION *fun = (CT_FUNCTION *)MHA(heap, 1, sizeof(CT_FUNCTION));

  result->tag = CT_T_FUNCTION;
  result->info.function = fun;

  if ( copyFunName == BTRUE )
      fun->fun_name = libStrdup(heap, funName);
  else
      fun->fun_name = funName;

  if ( !macros ) {
      fun->macros = (CT_PHRASE **)MHA(heap, 1, sizeof(CT_PHRASE *));
      fun->macros[0] = NULL;
  }   /*  fi  */
  else
      fun->macros = macros;

  return result;

}   /*  end ctMakeFunTerm  */


/************************************
 *                                  *
 *    ctMakeVarExpr                 *
 *                                  *
 ************************************/
CT_EXPR *
ctMakeVarExpr(MEMORY heap, char *var, BBOOL copyVar) {

  /* if var is NULL then return () */

  CT_EXPR *result = (CT_EXPR *)MHA(heap, 1, sizeof(CT_EXPR));

  if ( var ) {
      result->tag = CT_VAR;
  
      if ( copyVar == BTRUE ) 
          result->info.var = libStrdup(heap, var);
      else
          result->info.var = var;
  }   /*  fi  */
  else
      result->tag = CT_BANG;

  return result;

}


/************************************
 *                                  *
 *    ctMakeBangExpr                *
 *                                  *
 ************************************/
CT_EXPR *
ctMakeBangExpr(MEMORY heap) {

  CT_EXPR *result;

  result = (CT_EXPR *)MHA(heap, 1, sizeof(CT_EXPR));
  result->tag = CT_BANG;
  
  return result;

}   /*  end ctMakeBangExpr  */


/*********************************
 *                               *
 *    ctMakePairExpr             *
 *                               *
 *********************************/
CT_EXPR *
ctMakePairExpr(MEMORY heap, CT_EXPR *exprL, CT_EXPR *exprR) {

    CT_EXPR   *result = (CT_EXPR *)MHA(heap, 1, sizeof(CT_EXPR));

    result->tag = CT_PAIR;
    result->info.pair.l = exprL;
    result->info.pair.r = exprR;

    return result;

}   /*  end ctMakePairExpr  */


/*********************************
 *                               *
 *    ctMakeAbsTerm              *
 *                               *
 *********************************/
CT_TERM *
ctMakeAbsTerm(MEMORY heap, CT_VAR_BASE *vb, CT_EXPR *rhs) {

  CT_TERM       *result = (CT_TERM *)MHA(heap, 1, sizeof(CT_TERM));
  CT_ABS        *abst = (CT_ABS *)MHA(heap, 1, sizeof(CT_ABS));

  result->tag = CT_T_ABS;
  result->info.abs = abst;

  abst->var_base = vb;
  abst->expr     = rhs;

  return result;

}   /*  end ctMakeAbsTerm  */


/*********************************
 *                               *
 *    ctMakeAPPExpr              *
 *                               *
 *********************************/
CT_EXPR *
ctMakeAPPExpr(MEMORY heap, CT_TERM *term, CT_EXPR *expr) {

  CT_EXPR *result = (CT_EXPR *)MHA(heap, 1, sizeof(CT_EXPR));

  result->tag = CT_APP;
  result->info.app.term = term;
  result->info.app.expr = expr;

  return result;

}   /*  end ctMakeAPPExpr  */

#if 0
***************************************************************************
/*********************************
 *                               *
 *    ctPtrsInExpr               *
 *                               *
 *********************************/
VOIDPTR_LIST *
ctPtrsInExpr(CT_EXPR *expr, VOIDPTR_LIST (*(* exprFuns[])())) {

    /* returns a list of pointers  */
    BBOOL               saveIt = BFALSE;
    VOIDPTR_LIST       *ptrList = NULL;

    switch ( expr->tag ) {
    case CT_VAR:
        ptrList = (* exprFuns[CT_VAR])(&saveIt, expr->info.var);
        break;
    case CT_BANG:
        ptrList = (* exprFuns[CT_BANG])(&saveIt);
        break;
    case CT_PAIR:
        ptrList = (* exprFuns[CT_PAIR])(&saveIt, expr->info.pair.l, expr->info.pair.r);
        break;
    case CT_APP:
        ptrList = (* exprFuns[CT_APP])(&saveIt, expr->info.app.term, expr->info.app.expr);
        break;
    default:
        assert(NULL);
    }   /*  hctiws  */

    if ( saveIt == BTRUE )
        return VoidPtrListCons(&expr, ptrList);
    else
        return ptrList;

}   /*  end ctPtrsInExpr  */


/*********************************
 *                               *
 *    ctPtrsInTerm               *
 *                               *
 *********************************/
INT_LIST *
ctPtrsInTerm(CT_TERM *term, structFun, funFun, macroFun, mapFun, foldFun, unfoldFun, caseFun, recFun, absFun) {
    /* returns a list of pointers  */

    INT_LIST           *ptrList = NULL;
    BBOOL               saveIt = BFALSE;

    switch ( term->tag ) {
    case CT_T_STRUCTOR:
        ptrList = structFun(&saveIt, term->info.struct_name);
        break;
    case CT_T_FUNCTION :
        ptrList = funFun(&saveIt, term->info.function);
        break;
    case  CT_T_MACRO :
        ptrList = macroFun(&saveIt, term->info.macro);
        break;
    case CT_T_MAP :
        ptrList = mapFun(&saveIt, term->info.maps);
        break;
    case CT_T_FOLD :
        ptrList = foldFun(&saveIt, term->info.folds);
        break;
    case CT_T_UNFOLD :
        ptrList = unfoldFun(&saveIt, term->info.unfolds);
        break;
    case CT_T_CASE :
        ptrList = caseFun(&saveIt, term->info.cases);
        break;
    case CT_T_RECORD :
        ptrList = recFun(&saveIt, term->info.records);
        break;
    case CT_T_ABS :
        ptrList = absFun(&saveIt, term->info.abs);
        break;
    default:
        assert(NULL);
    }   /*  hctiws  */

    if ( saveIt == BTRUE )
        return Int_ListCons(&expr, ptrList);
    else
        return ptrList;

}   /*  end ctPtrsInExpr  */
***************************************************************************
#endif

/*********************************
 *                               *
 *    ctExprExpr                 *
 *                               *
 *********************************/
CT_LIST_EXPRADDR *
ctExprExpr(MEMORY heap, CT_EXPR **expr1Ptr, CT_EXPR *expr2) {
/* returns a list of pointers to expr2 found in expr1 */

    CT_EXPR   *expr1 = *expr1Ptr;

    if ( ctExprCmp(expr1, expr2) == BTRUE )
        return CTExprAddrListCons(expr1Ptr, NULL);

    switch ( expr1->tag ) {
    case CT_VAR:
    case CT_BANG:
        return NULL;
        break;
    case CT_PAIR:
        return CTExprAddrListAppend(ctExprExpr(heap,&expr1->info.pair.l,expr2),
                                  ctExprExpr(heap,&expr1->info.pair.r, expr2));
        break;
    case CT_APP:
      return CTExprAddrListAppend(ctTermExpr(heap, expr1->info.app.term,expr2),
                                 ctExprExpr(heap,&expr1->info.app.expr,expr2));
        break;
    default:
        assert(NULL);
    }   /*  hctiws  */

}   /*  end ctExprExpr  */


/*********************************
 *                               *
 *    ctTermExpr                 *
 *                               *
 *********************************/
CT_LIST_EXPRADDR *
ctTermExpr(MEMORY heap, CT_TERM *term, CT_EXPR *expr) {
/* returns a list of pointers to expr found in term */

    CT_LIST_EXPRADDR   *result = NULL;
    CT_PHRASE          *phr = NULL;
    CT_MAP_PHRASE      *mphr = NULL;
    CT_FOLD            *fld = NULL;
    CT_UNFOLD          *unfld = NULL;
    CT_CASE            *cse = NULL;
    CT_RECORD          *rec = NULL;
    int                 i = 0;

    switch ( term->tag ) {
    case CT_T_STRUCTOR :
        result = NULL;
        break;
    case CT_T_FUNCTION :
        while ( phr = term->info.function->macros[i++] )
            result = CTExprAddrListAppend(ctExprExpr(heap, &phr->expr, expr),
                                          result);
        break;
    case CT_T_MACRO :
        while ( phr = term->info.macro->macros[i++] )
            result = CTExprAddrListAppend(ctExprExpr(heap, &phr->expr, expr),
                                          result);
        break;
    case CT_T_MAP :
        while ( mphr = term->info.maps->phrases[i++] ) {
            if ( mphr->expr )
                result = 
                    CTExprAddrListAppend(ctExprExpr(heap, &mphr->expr, expr),
                                         result);

            if ( mphr->neg_expr )
                result = 
                    CTExprAddrListAppend(ctExprExpr(heap,&mphr->neg_expr,expr),
                                         result);
        }   /*  elihw  */
        break;
    case CT_T_FOLD :
        while ( fld = term->info.folds[i++] )
            result = CTExprAddrListAppend(ctExprExpr(heap, &fld->expr, expr),
                                          result);
        break;
    case CT_T_UNFOLD :
        while ( unfld = term->info.unfolds[i++] )
            result = CTExprAddrListAppend(ctExprExpr(heap, &unfld->expr, expr),
                                          result);
        break;
    case CT_T_CASE :
        while ( cse = term->info.cases[i++] )
            result = CTExprAddrListAppend(ctExprExpr(heap, &cse->expr, expr),
                                          result);
        break;
    case CT_T_RECORD :
        while ( rec = term->info.records[i++] )
            result = CTExprAddrListAppend(ctExprExpr(heap, &rec->expr, expr),
                                          result);
        break;
    case CT_T_ABS :
        result = ctExprExpr(heap, &term->info.abs->expr, expr);
        break;
    default:
        assert(NULL);

    }   /*  hctiws  */

    return result;

}   /*  end ctTermExpr  */


/*********************************
 *                               *
 *    ctExprCmp                  *
 *                               *
 *********************************/
BBOOL
ctExprCmp(CT_EXPR *expr1, CT_EXPR *expr2) {
/* expr1 & expr2 cannot be NULL */
/* returns true if expr1 is the same as expr2                            *
 * "same" is defined as having the same structure with equivalent values *
 * for strings                                                           */

    /* can only handle variable expressions, so far */

    switch ( expr1->tag ) {
    case CT_VAR:
        if ( expr2->tag == CT_VAR ) {
            if ( strcmp(expr1->info.var, expr2->info.var) == 0 )
                return BTRUE;
        }   /*  fi  */
        return BFALSE;
        break;
    default:
        return BFALSE;
    }   /*  hctiws  */

}   /*  end ctExprCmp  */


/*********************************
 *                               *
 *    ct_isVarBase               *
 *                               *
 *********************************/
BBOOL 
ct_isVarBase(PE_PATT *patt) {

  BBOOL result;

  if (!patt) 
    return(BFALSE);

  switch (patt->tag) {
  case P_BANG   : 
  case P_VAR    : result = BTRUE;                              break;
  case P_PAIR   : result = ct_isVarBase(patt->info.ppair.l) &&
                           ct_isVarBase(patt->info.ppair.r);   break;
  case P_HOVAR  : 
  case P_RECORD : 
  case P_INT    :
  case P_STR    :
  case P_CHAR   :
  case P_CONSTR : result = BFALSE;                             break;
  default       :
    printMsg(FATAL_MSG, 
	     "ct_isVarBase - %d is not a valid PE_PATT tag", patt->tag);
  }   /*  hctiws  */

  return result;

}   /*  end ct_isVarBase  */


/*********************************
 *                               *
 *    CTExprListCons             *
 *                               *
 *********************************/
CT_LIST_EXPR
*CTExprListCons(CT_EXPR *x, CT_LIST_EXPR *list) {
     return((CT_LIST_EXPR *) _cons((char *) x, (LIST *) list, L_CT_EXPR, scratchHD));
}

/*********************************
 *                               *
 *    CTExprListHead             *
 *                               *
 *********************************/
CT_EXPR
*CTExprListHead(CT_LIST_EXPR *list)
{
     return((CT_EXPR *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    CTExprListTail             *
 *                               *
 *********************************/
CT_LIST_EXPR
*CTExprListTail(CT_LIST_EXPR *list) {
     return((CT_LIST_EXPR *) ListTail((LIST *) list));
}

/*********************************
 *                               *
 *    CTExprListLen              *
 *                               *
 *********************************/
int
CTExprListLen(CT_LIST_EXPR *list) {
     return( ListLen((LIST *) list) );
}


/*********************************
 *                               *
 *    CTExprListAppend           *
 *                               *
 *********************************/
CT_LIST_EXPR *
CTExprListAppend(CT_LIST_EXPR *l1, CT_LIST_EXPR *l2) {

     return((CT_LIST_EXPR *) _append((LIST *) l1, (LIST *) l2));
}


/*********************************
 *                               *
 *    CTExprAddrListCons         *
 *                               *
 *********************************/
CT_LIST_EXPRADDR *
CTExprAddrListCons(CT_EXPR **x, CT_LIST_EXPRADDR *list) {
     return((CT_LIST_EXPRADDR *) _cons((char *) x, (LIST *) list, L_CT_EXPRADDR, scratchHD));
}

/*********************************
 *                               *
 *    CTExprAddrListHead         *
 *                               *
 *********************************/
CT_EXPR **
CTExprAddrListHead(CT_LIST_EXPRADDR *list) {
     return((CT_EXPR **) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    CTExprAddrListTail         *
 *                               *
 *********************************/
CT_LIST_EXPRADDR *
CTExprAddrListTail(CT_LIST_EXPRADDR *list) {
     return((CT_LIST_EXPRADDR *) ListTail((LIST *) list));
}

/*********************************
 *                               *
 *    CTExprAddrListLen          *
 *                               *
 *********************************/
int
CTExprAddrListLen(CT_LIST_EXPRADDR *list) {
     return( ListLen((LIST *) list) );
}


/*********************************
 *                               *
 *    CTExprAddrListAppend       *
 *                               *
 *********************************/
CT_LIST_EXPRADDR *
CTExprAddrListAppend(CT_LIST_EXPRADDR *l1, CT_LIST_EXPRADDR *l2) {

     return((CT_LIST_EXPRADDR *) _append((LIST *) l1, (LIST *) l2));
}


/*********************************
 *                               *
 *    PMPhraseListCons             *
 *                               *
 *********************************/
PM_LIST_PHRASE
*PMPhraseListCons(PM_PHRASE *x, PM_LIST_PHRASE *list) {
     return((PM_LIST_PHRASE *) _cons((char *) x, (LIST *) list, L_PM_PHRASE, scratchHD));
}

/*********************************
 *                               *
 *    PMPhraseListHead             *
 *                               *
 *********************************/
PM_PHRASE
*PMPhraseListHead(PM_LIST_PHRASE *list) {
     return((PM_PHRASE *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    PMPhraseListTail             *
 *                               *
 *********************************/
PM_LIST_PHRASE
*PMPhraseListTail(PM_LIST_PHRASE *list) {
     return((PM_LIST_PHRASE *) ListTail((LIST *) list));
}


/*********************************
 *                               *
 *    PMPhraseListLen            *
 *                               *
 *********************************/
int
PMPhraseListLen(PM_LIST_PHRASE *list) {
     return( ListLen((LIST *) list) );
}


/************************************
 *                                  *
 *    PE_LIST_PATT functions        *
 *                                  *
 ************************************/
/* used during pm translation  */
PE_LIST_PATT *
PE_PattListCons(PE_PATT *x, PE_LIST_PATT *l){

  return((PE_LIST_PATT *) _cons((char *) x, (LIST *) l, L_PE_PATT, scratchHD));

}


PE_PATT *
PE_PattListHead(PE_LIST_PATT *list) {

  return((PE_PATT *) ListHead((LIST *) list));

}


PE_LIST_PATT *
PE_PattListTail(PE_LIST_PATT *list) {

  return((PE_LIST_PATT *) ListTail((LIST *) list));

}


PE_LIST_PATT *
PE_PattListAppend(PE_LIST_PATT *l1, PE_LIST_PATT *l2) {

     return((PE_LIST_PATT *) _append((LIST *) l1, (LIST *) l2));
}


PE_PATT *
PE_ListPattGetNthItem(int n, PE_LIST_PATT *patts) {

  return((PE_PATT *) ListGetNthElement(n, (LIST *)patts));

}


/************************************
 *                                  *
 *    INT_LIST functions            *
 *                                  *
 ************************************/
/* used during pm translation  */
INT_LIST *
Int_ListCons(int x, INT_LIST *l){

  return((INT_LIST *) _cons((char *) x, (LIST *) l, L_INT, scratchHD));

}


int
Int_ListHead(INT_LIST *list) {

  return((int) ListHead((LIST *) list));

}


INT_LIST *
Int_ListTail(INT_LIST *list) {

  return((INT_LIST *) ListTail((LIST *) list));

}


/************************************
 *                                  *
 *    VOIDPTR_LIST functions        *
 *                                  *
 ************************************/
/* used during pm translation  */
VOIDPTR_LIST *
VoidPtrListCons(void *x, INT_LIST *l){

  return((VOIDPTR_LIST *) _cons((char *) x, (LIST *) l, L_VOIDPTR, scratchHD));

}


void *
VoidPtrListHead(VOIDPTR_LIST *list) {

  return((void *) ListHead((LIST *) list));

}


VOIDPTR_LIST *
VoidPtrListTail(VOIDPTR_LIST *list) {

  return((VOIDPTR_LIST *) ListTail((LIST *) list));

}
