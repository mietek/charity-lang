/******************************************************************************
 *                                                                            *
 *   transExprs.c                                                             *
 *                                                                            *
 *   COPYRIGHT (c) 1995, 1996 by Charity Development Group.                   *
 *   All rights reserved.                                                     *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

/**************************************************************************
 *                                                                        *
 *           Code To Translate Charity Terms a.k.a. Expressions           *
 *                                                                        *
 **************************************************************************/

#include <string.h>
#include "pmPrivate.h"
#include "pm.h"
#include "lib.h"
#include "symtab.h"
#include "ioChar.h"

/**************************************************************************
 *                                                                        *
 *           Global variables                                             *
 *                                                                        *
 **************************************************************************/

int       pmkill_a_def;


/**************************************************************************
 *                                                                        *
 *           PROTOTYPES (INTERNAL ONLY)                                   *
 *                                                                        *
 **************************************************************************/
static CT_TERM         *transStructor(CT_TERM *result, PE_TERM *peTerm);
static CT_TERM         *transMacro(CT_TERM *result, PE_TERM *peTerm);
static CT_TERM         *transFunction(CT_TERM *result, PE_TERM *peTerm);
static CT_TERM         *transMap    (CT_TERM *result, PE_TERM *peTerm);
static CT_TERM         *transCase   (CT_TERM *result, PE_TERM *peExpr);
static CT_TERM         *transFold   (CT_TERM *result, PE_TERM *peTerm);
static CT_TERM         *transUnfold (CT_TERM *result, PE_TERM *peTerm);
static CT_TERM         *transRecord (CT_TERM *result, PE_TERM *peTerm);
static CT_TERM         *transBI     (CT_TERM *result, PE_TERM *peTerm);

static CT_EXPR *transPhrs(PE_LIST_T_PHRASE *phrases, CT_LIST_EXPR *rs);
static PM_LIST_PHRASE *prepPhrs(PE_LIST_T_PHRASE *phrases);

static PE_EXPR *makeUnfoldHOCaseExpr(PE_LIST_T_PHRASE *phrases,
                                     char *var, char *var2);
static PE_EXPR *makeProgExpr(PE_LIST_T_PHRASE *phrases, char *var);
static CT_PHRASE      **makeCorePhraseArray(PE_LIST_T_PHRASE **pePhrases);
static BBOOL            isComplete(CT_EXPR *expr);
static BBOOL            remXtraVar(char *var,        CT_EXPR *expr,
                                   char **resultVar, CT_EXPR **resultExpr);
static BBOOL            stripVar(CT_VAR_BASE **vb, CT_EXPR **expr);


/**************************************************************************
 *                                                                        *
 *           Function Definitions                                         *
 *                                                                        *
 **************************************************************************/

/*********************************
 *                               *
 *    transPeExpr                *
 *                               *
 *********************************/
CT_EXPR  *
transPeExpr(PE_EXPR  *peExpr){
/* begins the translation of parsed expressions to core term logic exprs */

  CT_EXPR *result = (CT_EXPR *)MemHeapAlloc(ctHD, 1, sizeof(CT_EXPR));
  CT_TERM *resultTerm = NULL;
  PE_TERM *peTerm = NULL;

  switch (peExpr->tag) {

    case E_VAR :
      result->tag = CT_VAR;
      result->info.var = libStrdup(ctHD, peExpr->info.var);
      break;

    case E_BANG :
      result->tag = CT_BANG;
      break;

    case E_PAIR :
      result->tag = CT_PAIR;
      result->info.pair.l = transPeExpr(peExpr->info.epair.l);
      result->info.pair.r = transPeExpr(peExpr->info.epair.r);
      break;

    case E_APP  :
        peTerm = peExpr->info.app.term;
        result->tag = CT_APP;
        result->info.app.expr = transPeExpr(peExpr->info.app.expr);

        result->info.app.term = (CT_TERM *)MHA(ctHD, 1, sizeof(CT_TERM));
        resultTerm = result->info.app.term;

        switch (peExpr->info.app.term->tag) {
        case T_COMPLETE_CASE :
            resultTerm = transCase(resultTerm, peTerm);

            if ( isComplete(result) == BFALSE ) {
                cleanup();
                /* clean out DEF from symbol table */
                tc_close_typechecker(pmkill_a_def);
                printMsg(ERROR_MSG, "Pattern matching - incomplete patterns.");
            }   /*  fi  */
            break;
        case T_CASE :
            resultTerm = transCase(resultTerm, peTerm);                break;
        case T_BUILTIN:
          resultTerm = transBI(resultTerm, peTerm);                    break;
        case T_STRUCTOR :
            resultTerm = transStructor(resultTerm, peTerm);            break;
        case T_FUNCTION :
            resultTerm = transFunction(resultTerm, peTerm);            break;
        case T_MACRO :
            resultTerm = transMacro(resultTerm, peTerm);               break;
        case T_MAP :
            resultTerm = transMap(resultTerm, peTerm);                 break;
        case T_FOLD :
            resultTerm= transFold(resultTerm, peTerm);                 break;
        case T_UNFOLD :
            resultTerm = transUnfold(resultTerm, peTerm);              break;
        case T_RECORD :
            resultTerm = transRecord(resultTerm, peTerm);              break;
        default :
            printMsg(FATAL_MSG,"transPeExpr", "%d is invalid tag for a term",
                     peTerm->tag);
        }   /*  hctiws  */

        break;

    default :
      printMsg(FATAL_MSG,"transPeExpr", "%d is invalid tag here.",peExpr->tag);
    }   /*  hctiws  */

  return result;

}   /*  end transPeExpr  */


/*********************************
 *                               *
 *    transStructor              *
 *                               *
 *********************************/
static CT_TERM *
transStructor(CT_TERM *result, PE_TERM *peTerm) {

    result->tag = CT_T_STRUCTOR;
    result->info.struct_name = libStrdup(ctHD, peTerm->info.struct_name);

    return result;

}   /*  end transStructor  */


/*********************************
 *                               *
 *    transBI                    *
 *                               *
 *********************************/
static CT_TERM *
transBI(CT_TERM *result, PE_TERM *peTerm) {

PE_BUILTIN *bi = peTerm->info.builtin;

    result->tag = CT_T_BUILTIN;

    switch ( bi->tag ) {
    case BI_INT :
      result->info.builtin = (CT_BUILTIN *)MHA (ctHD, 1, sizeof (CT_EXPR));
      result->info.builtin->tag    = CT_INT;
      result->info.builtin->info.i = (int)bi->info.intBI;
      break;

    case BI_CHAR :
      result->info.builtin = (CT_BUILTIN *)MHA (ctHD, 1, sizeof (CT_EXPR));
      result->info.builtin->tag    = CT_CHAR;
      result->info.builtin->info.c = bi->info.charBI;
      break;

    default :
        assert(NULL);
    }   /*  hctiws  */

    return result;

}    /*  end transBI()  */


/*********************************
 *                               *
 *    transFunction              *
 *                               *
 *********************************/
static CT_TERM *
transFunction(CT_TERM *result, PE_TERM *peTerm) {

  result->tag = CT_T_FUNCTION;
  result->info.function = (CT_FUNCTION *)MHA(ctHD, 1, sizeof(CT_FUNCTION));

  result->info.function->fun_name =
    libStrdup(ctHD, peTerm->info.function->fun_name);

  /* Set up an abstraction for each macro in the function */
  result->info.function->macros =
      makeCorePhraseArray(peTerm->info.function->macros);

  return result;

}   /*  end transFunction  */


/*********************************
 *                               *
 *    transMacro                 *
 *                               *
 *********************************/
static CT_TERM *
transMacro(CT_TERM *result, PE_TERM *peTerm) {

  result->tag = CT_T_MACRO;
  result->info.macro = (CT_MACROS *)MHA(ctHD, 1, sizeof(CT_MACROS));

  result->info.macro->macro_name =
      libStrdup(ctHD, peTerm->info.macro->macro_name);

  /* ((2)) Set up an abstraction for each macro in the macro */
  result->info.macro->macros = makeCorePhraseArray(peTerm->info.macro->macros);

  return(result);

}   /*  end transMacro  */


/*********************************
 *                               *
 *    transMap                   *
 *                               *
 *********************************/
static CT_TERM *
transMap (CT_TERM *result, PE_TERM *peTerm) {

  PE_MAP_PHRASE *pePhrases = peTerm->info.maps->phrases;
  int            numParams = getNumParams(peTerm->info.maps->type_name);
  int   index    = 0;
  char         *newVar = makeNewRsrvdVar(ctHD);
  CT_VAR_BASE  *varBase = ctMakeVarBase(ctHD, newVar, BFALSE);
  CT_MAP_PHRASE **pArray = (CT_MAP_PHRASE **)MHA(ctHD, numParams + 1,
                                                 sizeof(CT_MAP_PHRASE *));

  result->tag       = CT_T_MAP;
  result->info.maps = (CT_MAP *)MHA(ctHD, 1, sizeof (CT_MAP));
  result->info.maps->phrases = pArray;
  result->info.maps->type_name = libStrdup(ctHD, peTerm->info.maps->type_name);

  /* Set up an abstraction for each parameter in the type */
  for (index = 0; index < numParams; index++)    {
      pArray[index] = (CT_MAP_PHRASE *)MHA (ctHD, 1, sizeof (CT_MAP_PHRASE));

      pArray[index]->var_base = pArray[index]->neg_var_base = NULL;
      pArray[index]->expr = pArray[index]->neg_expr = NULL;

      if (pePhrases[index].positive)    {
          pArray[index]->var_base = varBase;
          pArray[index]->expr =
              transPeExpr(makeProgExpr(pePhrases[index].positive, newVar));
          stripVar(&(pArray[index]->var_base), &(pArray[index]->expr));
      }   /*  fi  */

      if (pePhrases[index].negative)    {
          pArray[index]->neg_var_base = varBase;
          pArray[index]->neg_expr =
              transPeExpr(makeProgExpr(pePhrases[index].negative, newVar));
          stripVar(&(pArray[index]->neg_var_base), &(pArray[index]->neg_expr));
      }   /*  fi  */

  }   /*  rof  */

  pArray[index] = NULL;

  return result;

}   /*  end transMap  */


/*********************************
 *                               *
 *    transCase                  *
 *                               *
 *********************************/
static CT_TERM *
transCase(CT_TERM *result, PE_TERM *peTerm) {

    char           *newVar = makeNewRsrvdVar(ctHD);
    CT_LIST_EXPR   *rs = NULL;
    CT_EXPR        *resultExpr = NULL;

    rs = CTExprListCons(ctMakeVarExpr(ctHD, newVar, BFALSE), NULL);
    resultExpr = transPhrs(peTerm->info.cases, rs);
    remXtraVar(newVar, resultExpr, &newVar, &resultExpr);

    result->tag = CT_T_ABS;
    result->info.abs = (CT_ABS *)MHA(ctHD,1,sizeof(CT_ABS));
    result->info.abs->var_base = ctMakeVarBase(ctHD,newVar,BFALSE);
    result->info.abs->expr = resultExpr;

    return result;

}   /*  end transCase  */


/*********************************
 *                               *
 *    transFold                  *
 *                               *
 *********************************/
static CT_TERM *
transFold(CT_TERM *result, PE_TERM *peTerm) {

  char        *parent = getStructorParent(peTerm->info.folds[0]->constr);
  int          numStructs = getNumStructors(parent);
  int          i = 0;
  CT_FOLD     *foldPhrase = NULL;
  PE_FOLD     *foldPePhrase = NULL;
  char         *newVar = makeNewRsrvdVar(ctHD);
  CT_VAR_BASE  *varBase = ctMakeVarBase(ctHD, newVar, BFALSE);

  result->tag = CT_T_FOLD;

  /* ((1)) Set up a fold phrase for each constructor of the type */
  result->info.folds = (CT_FOLD **)MHA(ctHD, numStructs+1, sizeof(CT_FOLD *));

  for(i=0;i<numStructs;i++) {    /* for each constructor do...  */

    foldPePhrase = peTerm->info.folds[i];

    foldPhrase = (CT_FOLD *)MHA(ctHD, 1, sizeof(CT_FOLD));
    foldPhrase->constr = libStrdup(ctHD, foldPePhrase->constr);
    foldPhrase->var_base = varBase;
    foldPhrase->expr = transPeExpr(makeProgExpr(foldPePhrase->phrases,newVar));
    stripVar(&foldPhrase->var_base, &foldPhrase->expr);

    result->info.folds[i] = foldPhrase;
  }   /*  rof  */
  result->info.folds[numStructs] = NULL;

  return(result);

}   /*  end transFold  */


/*********************************
 *                               *
 *    transUnfold                *
 *                               *
 *********************************/
static
CT_TERM *
transUnfold (CT_TERM *result, PE_TERM *peTerm) {
  char         *parent = getStructorParent(peTerm->info.unfolds[0]->destr);
  int           numStructs = getNumStructors(parent);

  char         *newVar = makeNewRsrvdVar(ctHD);
  CT_VAR_BASE  *varBase = ctMakeVarBase(ctHD, newVar, BFALSE);
  char         *newVar2 = NULL;
  CT_VAR_BASE  *varBase2 = NULL;

  int           i = 0;
  CT_UNFOLD    *unfoldPhrase = NULL;
  PE_UNFOLD    *phr = NULL;
  PE_EXPR      *caseExpr = NULL;

  result->tag = CT_T_UNFOLD;
  result->info.unfolds =
      (CT_UNFOLD **)MHA(ctHD,numStructs+1,sizeof(CT_UNFOLD *));

  for( i=0; i<numStructs; i++) {    /* for each destructor do...  */

    phr = peTerm->info.unfolds[i];

    unfoldPhrase = (CT_UNFOLD *)MHA(ctHD, 1, sizeof(CT_UNFOLD));
    unfoldPhrase->destr = libStrdup(ctHD, phr->destr);
    unfoldPhrase->var_base = varBase;

    if ( st_IsHO(st_NameToKey(phr->destr)) == BTRUE ) {
        if ( varBase2 == NULL ) {
            newVar2 = makeNewRsrvdVar(ctHD);
            varBase2 =  ctMakeVarBase(ctHD, newVar2, BFALSE);
        }   /*  fi  */
        unfoldPhrase->var_base2 = varBase2;
        caseExpr = makeUnfoldHOCaseExpr(phr->phrases, newVar, newVar2);
        unfoldPhrase->expr = transPeExpr(caseExpr);
    }   /*  fi  */
    else {
        unfoldPhrase->var_base2 = NULL;
        unfoldPhrase->expr = transPeExpr(makeProgExpr(phr->phrases, newVar));
    }   /*  esle  */

    /* stripping is not done for unfolds. It could be, but it is more *
     * complicated than for other functions                           */

    result->info.unfolds[i] = unfoldPhrase;
  }   /*  rof  */
  peTerm->info.unfolds[numStructs] = NULL;

  return result;

}   /*  end transUnfold  */


/*********************************
 *                               *
 *    transRecord                *
 *                               *
 *********************************/
static CT_TERM *
transRecord(CT_TERM *result, PE_TERM *peTerm) {

  /*  The parser makes sure that the record has contains all the
      destructors and that they have the same parent.
      The parser also makes sure that HO destructors take cases
      as their right hand side & that FO destructors take terms
      as their right hand side.
   */

  char *parent = getStructorParent(peTerm->info.records[0]->destr);
  int numStructs = getNumStructors(parent);
  int           i = 0;
  char         *newVar = NULL;
  CT_VAR_BASE  *varBase = NULL;
  PE_RECORD   *rec = NULL;

  result->tag = CT_T_RECORD;
  result->info.records =
    (CT_RECORD **)MHA(ctHD,numStructs+1,sizeof(CT_RECORD *));

  for ( i=0; i<numStructs; i++ ) {    /* for each destructor do...  */
    rec = peTerm->info.records[i];

    result->info.records[i] = (CT_RECORD *)MHA(ctHD,1,sizeof(CT_RECORD));
    result->info.records[i]->destr = libStrdup(ctHD, rec->destr);

    if ( st_IsHO(st_NameToKey(rec->destr)) == BTRUE ) {
        if ( varBase == NULL ) {
            newVar = makeNewRsrvdVar(ctHD);
            varBase = ctMakeVarBase(ctHD, newVar, BFALSE);
        }   /*  fi  */

        result->info.records[i]->var_base = varBase;
        result->info.records[i]->expr =
            transPeExpr(makeProgExpr(rec->cases->info.cases, newVar));
        stripVar(&result->info.records[i]->var_base,
                 &result->info.records[i]->expr);
    }   /*  fi  */
    else {
        result->info.records[i]->var_base = NULL;
        result->info.records[i]->expr = transPeExpr(rec->expr);
    }   /*  esle  */

  }   /*  rof  */
  result->info.records[numStructs] = NULL;

  return result;

}   /*  end transRecord  */


/*********************************
 *                               *
 *    transPhrs                  *
 *                               *
 *********************************/
static CT_EXPR *
transPhrs(PE_LIST_T_PHRASE *phrases, CT_LIST_EXPR *rs) {

    return pmTransPhrs(prepPhrs(phrases), rs);

}   /*  end transPhrs  */


/*********************************
 *                               *
 *    prepPhrs                   *
 *                               *
 * folds over phrases            *
 *                               *
 *********************************/
static PM_LIST_PHRASE *
prepPhrs(PE_LIST_T_PHRASE *phrases) {

    PE_LIST_T_PHRASE *tailPEPhrases = T_PhraseListTail(phrases);
    PM_LIST_PHRASE *tailPMPhrases = NULL;
    PE_T_PHRASE    *tPhrase = NULL;
    PM_PHRASE      *pmPhrase = NULL;
    PM_LIST_PHRASE *pmPhrases = NULL;

    if ( (tPhrase = T_PhraseListHead(phrases)) ) {
        pmPhrase = (PM_PHRASE *)MHA(scratchHD, 1, sizeof(PM_PHRASE));
        pmPhrase->patts = PE_PattListCons(tPhrase->patt, NULL);
        tailPMPhrases = prepPhrs(tailPEPhrases);

        /* inner case terms do not need to be programs */
        if (tPhrase->expr->tag == E_APP)
            if (tPhrase->expr->info.app.term->tag == T_COMPLETE_CASE)
                tPhrase->expr->info.app.term->tag = T_CASE;

        pmPhrase->rhs = transPeExpr(tPhrase->expr);
        pmPhrases = PMPhraseListCons(pmPhrase, tailPMPhrases);
    }   /*  fi  */
    else
        pmPhrases = NULL;

    return pmPhrases;

}   /*  end prepPhrs  */

/*********************************
 *                               *
 *    makeCorePhraseArray        *
 *                               *
 *********************************/
static CT_PHRASE **
makeCorePhraseArray(PE_LIST_T_PHRASE **pePhrases) {

  int         i          = 0;
  char         *newVar = makeNewRsrvdVar(ctHD);
  int         numPhrases = PtrArrayLen((char **)pePhrases);
  CT_PHRASE **pArray =(CT_PHRASE **)MHA(ctHD,numPhrases+1,sizeof(CT_PHRASE *));

  while (pePhrases[i]) {
    pArray[i] = (CT_PHRASE *)MHA(ctHD, 1, sizeof(CT_PHRASE));
    pArray[i]->var_base = ctMakeVarBase(ctHD, newVar, BFALSE);
    pArray[i]->expr = transPeExpr(makeProgExpr(pePhrases[i], newVar));
    i++;
  }   /*  elihw  */
  pArray[i] = NULL;

  return pArray;

}   /*  makeCorePhraseArray()  */


/*********************************
 *                               *
 *    makeUnfoldHOCaseTerm       *
 *                               *
 *********************************/
static PE_EXPR *
makeUnfoldHOCaseExpr(PE_LIST_T_PHRASE *phrases, char *var, char *var2) {

PE_T_PHRASE      *phr = NULL;
PE_LIST_T_PHRASE *tailPhrs = phrases;

  phr = T_PhraseListHead(tailPhrs);
  while ( phr ) {
      phr->expr = makeProgExpr(phr->cases->info.cases, var2);
      phr->expr->info.app.term->tag = T_CASE; /* doesn't need to be complete */
      phr->cases = NULL;
      tailPhrs = T_PhraseListTail(tailPhrs);
      phr = T_PhraseListHead(tailPhrs);
  }   /*  elihw  */

  return makeProgExpr(phrases, var);

}   /*  makeUnfoldHOCaseTerm()  */


/*********************************
 *                               *
 *    makeProgExpr               *
 *                               *
 *********************************/
static PE_EXPR *
makeProgExpr(PE_LIST_T_PHRASE *phrases, char *var) {

  PE_EXPR   *caseExpr = ExprNew(TermCaseNew(phrases), ExprVarNew(var));

  caseExpr->info.app.term->tag = T_COMPLETE_CASE;
  return caseExpr;

}   /*  makeProgExpr()  */


/*********************************
 *                               *
 *    isComplete                 *
 *                               *
 *********************************/
static BBOOL
isComplete(CT_EXPR *expr) {
/* The expr is incomplete if any part of it is incomplete */

  int          i = 0;
  CT_CASE    **cases = NULL;

  if ( expr->tag == CT_INCOMPLETE )
      return BFALSE;
  else if ( possiblyIncomplete(expr) == BTRUE ) {
      if ( expr->info.app.term->tag == CT_T_CASE ) {
          cases = expr->info.app.term->info.cases;
          while ( cases[i] ) {
              if ( isComplete(cases[i]->expr) == BFALSE )
                  return BFALSE;
              i++;
          }   /*  elihw  */
          return BTRUE;
      }   /*  fi  */
      else    /* must be an abstraction */
          return isComplete(expr->info.app.term->info.abs->expr);
  }   /*  esle fi  */
  else
      return BTRUE;

}   /*  isComplete()  */



/*********************************
 *                               *
 *    remXtraVar                 *
 *                               *
 *********************************/
static BBOOL
remXtraVar(char *var, CT_EXPR *expr,
           char **resultVar, CT_EXPR **resultExpr) {
/*   If input looks like:             *
 *      v,   { x => term } v          *
 *   then output looks like:          *
 *      x, term                       *
 *   otherwise output looks like:     *
 *      v,   expr                     *
 */

BBOOL result = BFALSE;

    if ( (expr->tag == CT_APP) &&
         (expr->info.app.term->tag == CT_T_ABS) &&
         (expr->info.app.term->info.abs->var_base->tag == CT_VB_VAR) &&
         (expr->info.app.expr->tag == CT_VAR) &&
         (strcmp(expr->info.app.expr->info.var, var) == 0) ) {
        *resultVar = expr->info.app.term->info.abs->var_base->info.var;
        *resultExpr = expr->info.app.term->info.abs->expr;
        result = BTRUE;
    }   /*  fi  */
    else {
        *resultVar = var;
        *resultExpr = expr;
        result = BFALSE;
    }   /*  esle  */

    return result;

}   /*  end remXtraVar()  */



/*********************************
 *                               *
 *    stripVar                   *
 *                               *
 *********************************/
static BBOOL
stripVar(CT_VAR_BASE **vb, CT_EXPR **expr) {

    char *var = NULL;
    char *stripVar = NULL;
    CT_EXPR *stripExpr = NULL;

    if ( ((*vb)->tag == CT_VB_VAR) )
        var = (*vb)->info.var;
    else
        return BFALSE;

    if ( remXtraVar(var, *expr, &stripVar, &stripExpr) == BTRUE ) {
        *vb = ctMakeVarBase(ctHD, stripVar, BFALSE);
        *expr = stripExpr;
        return BTRUE;
    }   /*  fi  */
    else
        return BFALSE;


}   /*  end stripVar()  */
