/******************************************************************************
 *                                                                            *
 *   transPhrases.c                                                           *
 *                                                                            *
 *   COPYRIGHT (c) 1995, 1996 by Charity Development Group.                   *
 *   All rights reserved.                                                     *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

/**************************************************************************
 *                                                                        *
 *           Code To Translate Pattern Matched Phrases                    *
 *                                                                        *
 **************************************************************************/

#include <stddef.h>
#include <string.h>
#include "ioChar.h"
#include "lib.h"
#include "list.h"
#include "parse.h"
#include "pm.h"
#include "pmPrivate.h"
#include "symtab.h"
#include "typecheck.h"
#include "types.h"




/**************************************************************************
 *                                                                        *
 * Prototypes (internal only) *
 *                                                                        *
 **************************************************************************/


static CT_EXPR  *terminalCase(PM_LIST_PHRASE *phrases);
static CT_EXPR  *varUnitCase(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs);
static CT_EXPR  *coCase(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs,
                           STR_LIST *structs);
static CT_EXPR *indCase(PM_LIST_PHRASE *phrases,CT_LIST_EXPR *rs,char *parent);
static CT_EXPR *intcharCase(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs,
                            char *intcharConst);

static STR_LIST *getStructs(PM_LIST_PHRASE *phrases);
static PE_LIST_PATT *makeDCList(int structsLen);
static PM_LIST_PHRASE *processPhrases(PM_LIST_PHRASE *phrases, CT_EXPR *r, 
                                      STR_LIST *destructs, char *constr,
                                      long i, BBOOL wanti);
static STR_LIST       *getDestrs(PE_PATT *p);
static PE_LIST_PATT   *getDestrPatts(PE_PATT *p, STR_LIST *structs);
static CT_LIST_EXPR   *applyStructsToR(STR_LIST *structs, CT_EXPR *r);

static CT_EXPR *replaceHOFunCalls(CT_EXPR *rhs, char *var, CT_EXPR *r, CT_TERM *term);

static PE_PATT        *getFirstPatt(PM_LIST_PHRASE *phrases, PE_PATT_TAG tag);

static CT_EXPR        *completeExpr(CT_EXPR *t_i, CT_EXPR *t_j);




/**************************************************************************
 *                                                                        *
 *           Function Definitions                                         *
 *                                                                        *
 **************************************************************************/

/*********************************
 *                               *
 *    pmTransPhrs               *
 *                               *
 *********************************/
CT_EXPR *
pmTransPhrs(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs) {

    CT_EXPR        *result = NULL;
    STR_LIST       *structs = NULL;
    char           *parent  = NULL;

    if ( !phrases ) {   /* out of phrases; put in INCOMPLETE EXPR */
        result = (CT_EXPR *)MHA(scratchHD, 1, sizeof(CT_EXPR));
        result->tag = CT_INCOMPLETE;
    }   /*  fi  */
    else if ( CTExprListLen(rs) == 0 ) {
        result = terminalCase(phrases);
    }   /*  fi  */
    else {
        structs = getStructs(phrases);
        if ( StrListLen(structs) == 0 ) {
            result = varUnitCase(phrases, rs);
        }   /*  fi  */
        else {
            parent = getStructorParent(StrListHead(structs));
            if ( isCoinductiveType(parent) == BTRUE ) {
                result = coCase(phrases, rs, structs);
            }
            else if ( strcmp(parent, INT_TYPENAME) == 0 ) {
                result = intcharCase(phrases, rs, INT_CONSTRUCTOR);
            }
            else if ( strcmp(parent, CHAR_TYPENAME) == 0 ) {
                result = intcharCase(phrases, rs, CHAR_CONSTRUCTOR);
            }
            else
                result = indCase(phrases, rs, parent);
        }   /*  esle  */
    }   /*  esle  */

    return result;

}   /*  end pmTransPhrs  */


/*********************************
 *                               *
 *    terminalCase               *
 *                               *
 *********************************/
static
CT_EXPR*
terminalCase(PM_LIST_PHRASE *phrases) {
/* phrases is not empty */
/* travels down the list of phrases completing rhss. A rhs only needs to
 * be completed if it is a case or an abstraction. All other terms are 
 * guaranteed to be complete. Once a complete rhs is found we can quit.
 */
  PM_PHRASE        *phr = PMPhraseListHead(phrases);
  PM_LIST_PHRASE   *tailPhrs = PMPhraseListTail(phrases);
  CT_EXPR          *result = phr->rhs;

  if ( possiblyIncomplete(result) == BTRUE ) {
      phr = PMPhraseListHead(tailPhrs);    /* get the 2nd phrase */
      tailPhrs = PMPhraseListTail(tailPhrs);
  }   /*  fi  */
  else
      phr = NULL;    /* result is complete */

  while ( phr ) {
     result = completeExpr(result, phr->rhs);
     if ( possiblyIncomplete(phr->rhs) == BTRUE ) {
         phr = PMPhraseListHead(tailPhrs);     /* get next phrase */
         tailPhrs = PMPhraseListTail(tailPhrs);
     }   /*  fi  */
     else      /* all other terms guaranteed to be complete */
         phr = NULL;
  }   /*  elihw  */

  return result;

}   /*  end *terminalCase  */



/*********************************
 *                               *
 *    coCase                  *
 *                               *
 *********************************/
static
CT_EXPR *
coCase(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs, STR_LIST *structs) {

    /* phrases can't be NULL           *
     * structs & dontCares may be NULL */
    CT_EXPR    *r = CTExprListHead(rs);
    phrases = processPhrases(phrases, r, structs, NULL, 0, BTRUE);

    return pmTransPhrs(phrases, CTExprListAppend(applyStructsToR(structs, r),
                                                 CTExprListTail(rs)));

}   /*  end *coCase  */



/*********************************
 *                               *
 *    varUnitCase                  *
 *                               *
 *********************************/
static
CT_EXPR *
varUnitCase(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs) {

    /* phrases can't be NULL           *
     * structs & dontCares may be NULL */
    CT_EXPR    *r = CTExprListHead(rs);
    phrases = processPhrases(phrases, r, NULL, NULL, 0, BTRUE);

    return pmTransPhrs(phrases, CTExprListTail(rs));

}   /*  end *varUnitCase  */



/*********************************
 *                               *
 *    indCase                    *
 *                               *
 *********************************/
static
CT_EXPR *
indCase(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs, char *parent) {

/* phrases, rs, parent cannot be NULL */

    char **structs = getStructorNames(parent);
    int numStructs = getNumStructors(parent);
    ST_TYPE *sType = NULL;
    CT_EXPR        *r = CTExprListHead(rs);
    PM_LIST_PHRASE *newPhrases = NULL;
    CT_EXPR        *result = (CT_EXPR *)MHA(ctHD, 1, sizeof(CT_EXPR));
    CT_CASE        *casePhrase = NULL;
    char           *varRsrvd = NULL;
    CT_VAR_BASE    *varBase = NULL;
    CT_EXPR        *varExpr = NULL;
    int             i = 0;

    /* Set up a case statement */
    result->tag = CT_APP;
    result->info.app.term = (CT_TERM *)MHA(ctHD, 1, sizeof(CT_TERM));
    result->info.app.term->tag = CT_T_CASE;
    result->info.app.term->info.cases = 
                        (CT_CASE **)MHA(ctHD, numStructs+1, sizeof(CT_CASE *));
         
    for ( i=0; i<numStructs; i++ ) {
        sType = getStructorType(structs[i]);
	casePhrase = (CT_CASE *)MHA(ctHD,1, sizeof(CT_CASE));

	casePhrase->constr = libStrdup(ctHD, structs[i]);

        /* build the variable base & discriminant list */
        if ( sType->tag != TYPE_1 ) {
            if ( varRsrvd == NULL ) {    /* need to create a variable */
                varRsrvd = makeNewRsrvdVar(ctHD);
                varBase = ctMakeVarBase(ctHD, varRsrvd, BFALSE);
                varExpr = ctMakeVarExpr(ctHD, varRsrvd, BFALSE);
            }   /*  fi  */

            casePhrase->var_base = varBase;
            rs = CTExprListCons(varExpr, CTExprListTail(rs));
        }   /*  fi  */
        else {
            casePhrase->var_base = bangVarBase;
            rs = CTExprListCons(bangExpr, CTExprListTail(rs));
        }   /*  esle  */

        newPhrases = processPhrases(phrases, r, NULL, structs[i], 0, BTRUE);
	casePhrase->expr = pmTransPhrs(newPhrases, rs);
	result->info.app.term->info.cases[i] = casePhrase;
    }   /*  rof  */
    result->info.app.term->info.cases[numStructs] = NULL;

    result->info.app.expr = r;

    return result;

}   /*  end indCase  */


/*********************************
 *                               *
 *    intcharCase                *
 *                               *
 *********************************/
static CT_EXPR *
intcharCase(PM_LIST_PHRASE *phrases, CT_LIST_EXPR *rs,
            char *intcharConst) {

    BBOOL       isIntConst = strcmp(intcharConst,INT_CONSTRUCTOR) == 0
                                ? BTRUE  : BFALSE;
    char        *charRep = NULL;
    CT_EXPR     *r = CTExprListHead(rs);
    PE_PATT     *firstIntPatt = getFirstPatt(phrases, 
                                             isIntConst
                                                 ? P_INT: P_CHAR);
    long         i = firstIntPatt->info.intcharBI.lTag == INTX 
                       ? firstIntPatt->info.intcharBI.l 
                       : firstIntPatt->info.intcharBI.u+1;
    CT_TERM     *intcharTerm  = (CT_TERM *)MHA (ctHD, 1, sizeof (CT_TERM));
    CT_EXPR     *iExpr = ctMakeAPPExpr(ctHD,intcharTerm, ctMakeBangExpr(ctHD));
    CT_TERM     *fun  = ctMakeFunTerm(ctHD, 
                                      isIntConst ? GE_INT : GE_CHAR,
                                      NULL, BTRUE);
    CT_EXPR     *funInput = ctMakePairExpr(ctHD, r, iExpr);
    CT_EXPR     *appExpr  = ctMakeAPPExpr(ctHD, fun, funInput);
    CT_TERM     *caseBool = (CT_TERM *)MHA(ctHD, 1, sizeof(CT_TERM));
    CT_EXPR     *result   = ctMakeAPPExpr(ctHD, caseBool, appExpr);

    if ( isIntConst == BFALSE ) {
        charRep = (char *)MHA(ctHD, 10, sizeof(char));
        sprintf(charRep, "%c", (char)i);
    }   /*  fi  */

    caseBool->tag = CT_T_CASE;
    caseBool->info.cases = (CT_CASE **)MHA(ctHD, 3, sizeof(CT_CASE *));

    caseBool->info.cases[0] = (CT_CASE *)MHA(ctHD, 1, sizeof(CT_CASE));
    caseBool->info.cases[0]->constr = libStrdup(ctHD, FALSE_CONSTRUCTORNAME);
    caseBool->info.cases[0]->var_base = ctMakeVarBase(ctHD, NULL, BFALSE);
    caseBool->info.cases[0]->expr = 
        pmTransPhrs(processPhrases(phrases, r, NULL, intcharConst, i, BTRUE),
                     rs);

    caseBool->info.cases[1] = (CT_CASE *)MHA(ctHD, 1, sizeof(CT_CASE));
    caseBool->info.cases[1]->constr = libStrdup(ctHD, TRUE_CONSTRUCTORNAME);
    caseBool->info.cases[1]->var_base = ctMakeVarBase(ctHD, NULL, BFALSE);
    caseBool->info.cases[1]->expr = 
        pmTransPhrs(processPhrases(phrases, r, NULL, intcharConst, i, BFALSE),
                     rs);
    caseBool->info.cases[2] = NULL;

    intcharTerm->tag          = CT_T_BUILTIN;
    intcharTerm->info.builtin = (CT_BUILTIN *)MHA(ctHD, 1, sizeof(CT_BUILTIN));
    intcharTerm->info.builtin->tag    = CT_INT;
    intcharTerm->info.builtin->info.i = (int)i;

    return result;

}   /*  end intcharCase()  */


/*********************************
 *                               *
 *    getStructs                 *
 *                               *
 *********************************/
static STR_LIST *
getStructs(PM_LIST_PHRASE *phrases) {

/* phrases can't be NULL */
    ST_KEY          parentK = 0;
    ST_KEY          sKey = 0;
    char          **structsType = NULL;
    int             i = 0;
    int             numStructs = 0;
    PM_PHRASE      *pm = NULL;
    PM_LIST_PHRASE *phrsTmp = phrases;
    PE_PATT        *p = NULL;
    int            *posns = NULL;
    STR_LIST       *structs = NULL;


    for (pm = PMPhraseListHead(phrsTmp); pm; pm = PMPhraseListHead(phrsTmp)) {

        p = PE_PattListHead(pm->patts);
        switch ( p->tag ) {
        case P_PAIR :
            structs = getDestrs(p);
            phrsTmp = NULL;
            break;
        case P_RECORD :
            structs = getDestrs(p);
            if ( structs != NULL ) {
                if ( posns == NULL ) {   /* this is first structor found */
                    sKey = st_NameToKey(StrListHead(structs));
                    parentK = st_GetStructorParent(sKey);
                    structsType = st_GetStructorNames(parentK);
                    numStructs = st_GetNumStructors(parentK);
                    posns = (int *)MHA(scratchHD, numStructs+1, sizeof(int));
                    posns[numStructs] = (int)NULL;
                }   /*  fi  */
                while ( structs ) {
                    sKey = st_NameToKey(StrListHead(structs));
                    posns[st_GetStructorPosn(sKey)]++;
                    structs = StrListTail(structs);
                }   /*  elihw  */
            }   /*  fi  */
            phrsTmp = PMPhraseListTail(phrsTmp);
            break;
        case P_CONSTR :
            structs = StrListCons(p->info.constr->id, NULL, scratchHD);
            phrsTmp = NULL;
            break;
        case P_VAR :
            phrsTmp = PMPhraseListTail(phrsTmp);
            break;
        case P_HOVAR : 
        case P_BANG :
            structs = NULL;
            phrsTmp = NULL;
            break;
        case P_INT :
            structs = StrListCons(INT_CONSTRUCTOR, NULL, scratchHD);
            phrsTmp = NULL;
            break;
        case  P_CHAR :
            structs = StrListCons(CHAR_CONSTRUCTOR, NULL, scratchHD);
            phrsTmp = NULL;
            break;
        default :
            assert(NULL);
        }   /*  hctiws  */

    
    }   /*  rof  */    
    
    if ( posns != NULL ) {
        for ( i=numStructs-1; i>=0; i-- ) 
            if ( posns[i] != 0 ) 
                structs = StrListCons(structsType[i], structs, scratchHD);
    }   /*  fi  */

    return structs;

}   /*  end getStructs  */


/*********************************
 *                               *
 *    makeDCList                 *
 *                               *
 *********************************/
static
PE_LIST_PATT *
makeDCList(int structsLen) {

    /* Share the don't cares because they won't  *
     * be in the end result anyways.             */
    int i = 0;

    PE_LIST_PATT   *result = NULL;
    PE_PATT        *dcPatt = peMakeVarPatt(scratchHD, DONTCARE, BTRUE);

    for ( i=0; i<structsLen; i++ ) 
        result = PE_PattListCons(dcPatt, result);

    return result;

}   /*  end makeDCList  */


/*********************************
 *                               *
 *    getDestrPatts              *
 *                               *
 *********************************/
static PE_LIST_PATT *
getDestrPatts(PE_PATT *p, STR_LIST *structs) {
    /* p must be a record or a pair pattern *
     * p is NULL iff structs is NULL        */
    PE_LIST_PATT *result = NULL;
    char         *s = NULL;
    P_STRUCTOR   *d = NULL;
    int           sPos = 0;
    int           i = 0;
    int           numStructs = 0;
    ST_KEY        sKey;
    PE_PATT     **patts = NULL;
    PE_PATT      *dcPatt = NULL;
    STR_LIST     *oldStructs = NULL;

    if ( structs == NULL )
        return NULL;

    if ( p->tag == P_PAIR ) {
        result = PE_PattListCons(p->info.ppair.r, NULL);
        result = PE_PattListCons(p->info.ppair.l, result);
        return result;
    }   /*  fi  */

    /* p->tag == P_RECORD */
    s = StrListHead(structs);
    sKey = st_NameToKey(s);
    numStructs = st_GetNumStructors(st_GetStructorParent(sKey));
    patts = (PE_PATT **)MHA(scratchHD, numStructs+1, sizeof(PE_PATT *));
    patts[numStructs] = NULL;

    while ( (d = p->info.record[i++]) ) {   
        /* load patts with destructor patterns from the record pattern */
        sPos = getStructorPosn(d->id);
        patts[sPos] = d->arg;
    }   /*  elihw  */

    /* WARNING: changes internal ptrs */
    oldStructs = structs = StrListReverse(structs); 
    while ( structs ) {
        /* create a list of patterns; one for each destructor in structs */
        s = StrListHead(structs);
        sPos = getStructorPosn(s);

        if ( patts[sPos] == NULL ) {
            if ( dcPatt == NULL )
                dcPatt = peMakeVarPatt(scratchHD, DONTCARE, BTRUE);
            result = PE_PattListCons(dcPatt, result);
        }   /*  fi  */
        else
            result = PE_PattListCons(patts[sPos], result);

        structs = StrListTail(structs);
    }   /*  elihw  */
    StrListReverse(oldStructs);  /* puts internal ptrs back to original posn */

    return result;

}   /*  end getDestrPatts  */


/*********************************
 *                               *
 *    applyStructsToR            *
 *                               *
 *********************************/
static
CT_LIST_EXPR *
applyStructsToR(STR_LIST *structs, CT_EXPR *r) {

/* structs may be NULL                                     *
 * If a struct is HO then don't apply it to r (just use r) */

    CT_LIST_EXPR     *result = NULL;
    CT_EXPR          *app = NULL;
    char             *s = StrListHead(structs);

    if ( structs == NULL )
        return NULL;

    if ( st_IsHO(st_NameToKey(s)) == BTRUE )
        app = r;
    else  
        app = ctMakeAPPExpr(ctHD, ctMakeStructTerm(ctHD, s, BTRUE), r);

    result = CTExprListCons(app, applyStructsToR(CTExprListTail(structs), r));

    return result;

}   /*  end appStructsToR  */



/*********************************
 *                               *
 *    processPhrases             *
 *                               *
 *********************************/
static
PM_LIST_PHRASE *
processPhrases(PM_LIST_PHRASE *phrases, CT_EXPR *r, STR_LIST *destructs, 
               char *constr, long i, BBOOL trueCase) { 

    /* The pattern lists in phrases cannot be NULL                      */

    PM_LIST_PHRASE *result = NULL;
    PM_PHRASE     *pm = NULL;
    PE_PATT       *p = NULL;
    PE_LIST_PATT  *ps = NULL;
    PM_PHRASE     *pmNew = NULL;
    CT_EXPR       *rhsNew = NULL;

    PE_INT_TAG     lTag = NEGINF,  
                   uTag = POSINF;
    long           l = 0, 
                   u = 0;
    PE_PATT       *pNew = NULL;
    CT_TERM       *dTerm = NULL;

    if ( phrases == NULL )
        return NULL;

    result = processPhrases(PMPhraseListTail(phrases), r, destructs, constr,
                            i, trueCase);

    pm = PMPhraseListHead(phrases);
    p = PE_PattListHead(pm->patts);   /* patts cannot be NULL */
    ps = PE_PattListTail(pm->patts);   /* patts cannot be NULL */
    pmNew = (PM_PHRASE *)MHA(scratchHD, 1, sizeof(PM_PHRASE));

   switch ( p->tag ) {
    case P_VAR :
        if ( strcmp(p->info.var, DONTCARE) != 0 ) 
            /* substitute r for var on rhs */
            rhsNew = ctMakeAPPExpr(ctHD,ctMakeAbsTerm(ctHD,ctMakeVarBase(ctHD,
                                          p->info.var, BTRUE), pm->rhs),    r);
        else
            rhsNew = pm->rhs;

        if ( constr )       /* processing constr. patts & building new list */
            pmNew->patts = PE_PattListAppend(makeDCList(1), ps);
        else if ( destructs )   /* processing record patts */
            pmNew->patts =
               PE_PattListAppend(makeDCList(StrListLen(destructs)),ps);
        else   /* processing variable or unit patterns */
            pmNew->patts = ps;
        pmNew->rhs = rhsNew;
        result = PMPhraseListCons(pmNew, result);
        break;

    case P_HOVAR :

        /* can only have variables in place for higher order patterns */
        pmNew->patts = ps;
        if ( strcmp(p->info.var, DONTCARE) != 0 ) {
            dTerm = ctMakeStructTerm(ctHD, 
                        st_KeyToName(p->info.hovar.destr), BTRUE);  
            /* substitute <d_i(expr, r)> for <var expr> on rhs */
            pmNew->rhs =
                 replaceHOFunCalls(pm->rhs, p->info.hovar.hovar, r, dTerm);
        }   /*  fi  */
        else
            pmNew->rhs = pm->rhs;
        result = PMPhraseListCons(pmNew, result);
        break;

    case P_BANG :
        pmNew->patts = ps;
        pmNew->rhs = pm->rhs;
        result = PMPhraseListCons(pmNew, result);
        break;

    case P_RECORD :
    case P_PAIR   :
        pmNew->patts=PE_PattListAppend(getDestrPatts(p, destructs), ps);
        pmNew->rhs = pm->rhs;
        result = PMPhraseListCons(pmNew, result);
        break;

    case P_CONSTR :
       if ( strcmp(p->info.constr->id, constr) == 0 ) {
            pmNew = (PM_PHRASE *)MHA(scratchHD, 1, sizeof(PM_PHRASE));
            pmNew->patts = PE_PattListCons(p->info.constr->arg, ps);
            pmNew->rhs = pm->rhs;
            result = PMPhraseListCons(pmNew, result);
        }   /*  fi  */
        break;

    case P_STR :
        if ( (trueCase && (strcmp(p->info.strBI, constr) == 0)) ||
             (!trueCase && (strcmp(p->info.strBI, constr) != 0)) ) {
                           /* it's a keeper */
                pmNew = (PM_PHRASE *)MHA(scratchHD, 1, sizeof(PM_PHRASE));
                if ( trueCase )
                    pmNew->patts = PE_PattListAppend(makeDCList(1), ps);
                else
                    pmNew->patts = PE_PattListCons(peCopyPatt(scratchHD,p),ps);
                pmNew->rhs = pm->rhs;
                result = PMPhraseListCons(pmNew, result);
        }   /*  fi  */
        break;

    case P_CHAR :
    case P_INT :
        /* (trueCase == BTRUE) means (r < i) */
        /* (trueCase == BFALSE) means (r >= i) */
        lTag = p->info.intcharBI.lTag;
        uTag = p->info.intcharBI.uTag;

        /* pattern covers all possible integers */
        if ( (lTag == NEGINF) && (uTag == POSINF) ) {
            pmNew = (PM_PHRASE *)MHA(scratchHD, 1, sizeof(PM_PHRASE));
            pmNew->patts = PE_PattListAppend(makeDCList(1), ps);
            pmNew->rhs = pm->rhs;
            result = PMPhraseListCons(pmNew, result);
            break;
        }   /*  esle  */

        /* r will never match the pattern */
        l = p->info.intcharBI.l;
        u = p->info.intcharBI.u;
        if ( ( !trueCase && (uTag == INTX) && (u < i)) ||
             (trueCase && (lTag == INTX) && (l >= i)) )
                break;   /* drop this phrase (won't match) */

        /* r lies between the upper & lower bounds of the pattern.
         * Therefore, one side can be increased to infinity.
         */
        if ( (!trueCase && (lTag == INTX) && (l <= i)) ||
             (trueCase  && (uTag == INTX) && (u >= i-1)) ) {
            pmNew = (PM_PHRASE *)MHA(scratchHD, 1, sizeof(PM_PHRASE));
            pNew = peCopyPatt(scratchHD, p);
            pmNew->patts = PE_PattListCons(pNew, ps);
            pmNew->rhs = pm->rhs;
            if ( trueCase == BFALSE )
                pNew->info.intcharBI.lTag = NEGINF;
            else
                pNew->info.intcharBI.uTag = POSINF;
            result = PMPhraseListCons(pmNew, result);
            break;
        }   /*  fi  */

        /* The pattern is wholly on one side of the discriminant */
        result = PMPhraseListCons(pm, result);
        break;

    }   /*  hctiws  */

    return result;

}   /*  end processPhrases  */



/*********************************
 *                               *
 *    getDestrs                  *
 *                               *
 *********************************/
static 
STR_LIST *
getDestrs(PE_PATT *p) {

    /* p must be a record or a pair pattern   *
     * The order of the list is not important */

    P_STRUCTOR    **structs = p->info.record;
    STR_LIST       *destrs = NULL;
    P_STRUCTOR     *d = NULL;
    int             i = 0;

    if ( p->tag == P_PAIR ) {
        destrs = StrListCons(PROD1, destrs, scratchHD);
        destrs = StrListCons(PROD0, destrs, scratchHD);
    }   /*  fi  */
    else {
        while ( (d = structs[i++]) ) 
            destrs = StrListCons(d->id, destrs, scratchHD);
    }   /*  esle  */

    return destrs;

}   /*  end getDestrs  */


/*********************************
 *                               *
 *    replaceHOFunCalls          *
 *                               *
 *********************************/
static 
CT_EXPR *
replaceHOFunCalls(CT_EXPR *rhs, char *var, CT_EXPR *r, CT_TERM *term) {

    CT_PHRASE          *phr = NULL;
    CT_MAP_PHRASE      *mphr = NULL;
    CT_FOLD            *fld = NULL;
    CT_UNFOLD          *unfld = NULL;
    CT_CASE            *cse = NULL;
    CT_RECORD          *rec = NULL;
    CT_TERM            *rhsTerm = NULL;
    int                 i = 0;

    if ( rhs == NULL )
        return NULL;

    switch ( rhs->tag ) {
    case CT_VAR :
    case CT_BANG :
        return rhs;
    case CT_APP :
        rhs->info.app.expr = replaceHOFunCalls(rhs->info.app.expr, var,r,term);

        rhsTerm = rhs->info.app.term;

        switch ( rhsTerm->tag ) {
        case CT_T_STRUCTOR :
        case CT_T_BUILTIN :
            break;
        case CT_T_FUNCTION :
            if ( strcmp(rhsTerm->info.function->fun_name, var) == 0 ) {
                rhs = ctMakeAPPExpr(ctHD, term, 
                                    ctMakePairExpr(ctHD,rhs->info.app.expr,r));
            }   /*  fi  */
            while ( (phr = rhsTerm->info.function->macros[i++]) )
                phr->expr = replaceHOFunCalls(phr->expr,var, r, term);
            break;
        case CT_T_MACRO :
            while ( (phr = rhsTerm->info.macro->macros[i++]) )
                phr->expr = replaceHOFunCalls(phr->expr,var, r, term);
            break;
        case CT_T_MAP :
            while ( (mphr = rhsTerm->info.maps->phrases[i++]) ) {
                mphr->expr = replaceHOFunCalls(mphr->expr,var, r, term);
                mphr->neg_expr = replaceHOFunCalls(mphr->neg_expr,var, r,term);
            }   /*  elihw  */
            break;
        case CT_T_FOLD :
            while ( (fld = rhsTerm->info.folds[i++]) )
                fld->expr = replaceHOFunCalls(fld->expr,var, r, term);
            break;
        case CT_T_UNFOLD :
            while ( (unfld = rhsTerm->info.unfolds[i++]) )
               unfld->expr = replaceHOFunCalls(unfld->expr,var, r, term);
            break;
        case CT_T_CASE :
            while ( (cse = rhsTerm->info.cases[i++]) )
                cse->expr = replaceHOFunCalls(cse->expr,var, r, term);
            break;
        case CT_T_RECORD :
            while ( (rec = rhsTerm->info.records[i++]) )
                rec->expr = replaceHOFunCalls(rec->expr,var, r, term);
            break;
        case CT_T_ABS :
            rhsTerm->info.abs->expr = 
                replaceHOFunCalls(rhsTerm->info.abs->expr,var, r, term);
            break;
        default:
            assert(NULL);
    
        }   /*  hctiws  */

        break;
    case CT_PAIR :
        rhs->info.pair.l = replaceHOFunCalls(rhs->info.pair.l, var, r, term);
        rhs->info.pair.r = replaceHOFunCalls(rhs->info.pair.r, var, r, term);
        break;
    default :
        assert(NULL);
    }   /*  hctiws  */

    return rhs;

}   /*  end replaceHOFunCalls  */

/*********************************
 *                               *
 *    getFirstPatt               *
 *                               *
 *********************************/
static  PE_PATT *
getFirstPatt(PM_LIST_PHRASE *phrases, PE_PATT_TAG tag) {

/* phrases must not be empty                       *
 * The patterns in phrases must not be empty lists */

    PE_PATT    *result = NULL;
    PE_PATT    *patt = NULL;

    while ( (phrases != NULL) && (result == NULL) ) {
        patt = PE_PattListHead(PMPhraseListHead(phrases)->patts);
        if ( patt->tag == tag )
            result = patt;
        phrases = PMPhraseListTail(phrases);
    }   /*  elihw  */

    return result;

}   /*  end getFirstPatt()  */



/*********************************
 *                               *
 *    completeExpr               *
 *                               *
 *********************************/
static
CT_EXPR *
completeExpr(CT_EXPR *t_i, CT_EXPR *t_j) {
/* fold over t_i */
  CT_CASE   **cases = NULL;
  int         i = 0;

  switch ( t_i->tag ) {
  case CT_INCOMPLETE:
      t_i = t_j;
      break;
  case CT_APP:
      if ( possiblyIncomplete(t_i) == BTRUE ) {
          if ( t_i->info.app.term->tag == CT_T_CASE ) {
              cases = t_i->info.app.term->info.cases;
              while ( cases[i] ) {
                  cases[i]->expr = completeExpr(cases[i]->expr, t_j);
                  i++;
              }   /*  elihw  */
          }   /*  fi  */
          else    /* must be an abstraction */
              t_i->info.app.term->info.abs->expr =  
                  completeExpr(t_i->info.app.term->info.abs->expr, t_j);
      }   /*  fi  */
      break;
  default:
      break;
  }   /*  hctiws  */

  return t_i;

}   /*  end completeExpr()  */

