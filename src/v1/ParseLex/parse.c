/******************************************************************************
 *                                                                            *
 *   parse.c                                                                  *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#include <limits.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>     /* [BI] ADDED THIS INCLUDE */
#include "lib.h"
#include "ioChar.h"
#include "types.h"
#include "parse.h"
#include "pmem.h"
#include "symtab.h"
#include "pm.h"
#include "variance.h"

#define   MAXPWORKSPACE 1000
#define   MHA           MemHeapAlloc

/**********************
 * Prototypes
 **********************/

/* externally visible declarations */
MEMORY       parseHeapDesc, prsHD;
PARSE_RESULT ParseResult;

BBOOL gbChangeScope = BTRUE;

/**********************/
static PE_VAR_BASE *pe_TranslateVBasePatt(PE_PATT *patt);

/* [H-O] ADDED THIS PROTOTYPE (SEE BELOW): */

static PE_LIST_TERM *Phrases2Terms (PE_LIST_FUN_PHRASE *phrases);

/* [H-O] ALTERED THE TYPE OF THE SECOND PARAMETER (SEE BELOW): */

static PE_TERM *TermMapNew (char *type, PE_LIST_FUN_PHRASE *phrases);

static PE_TERM *TermAliasMapNew (char               *alias,
				 PE_LIST_FUN_PHRASE *phrases);
static PE_TERM *MakeAliasMap    (V_VARIANCE          variance,
				 ST_TYPE            *expansion,
				 PE_LIST_FUN_PHRASE *phrases);

static PE_TERM      *TermFunNew(char *funName, PE_LIST_LIST_T_PHRASE *phrases);
static PE_TERM      *TermMacroNew(char *macroName, PE_LIST_TERM *terms);
static PE_TERM      *TermStructorNew(char *structor);
static PE_LIST_T_PHRASE **makePhraseArray(PE_LIST_TERM *terms,int numElements);

/* [H-O] ADDED THIS PROTOTYPE (SEE BELOW): */

static PE_MAP_PHRASE *MakeMapPhraseArray (PE_LIST_FUN_PHRASE *funPhrases,
					  int                 numParams,
					  V_VARIANCE         *varity);

static char         *peReplaceVar(char *var, BBOOL isHO);

static PE_BUILTIN *peMakeBuiltIn(PE_BUILTIN_TAG BI_STRING, void *str);

/*********************************
 *                               *
 *    ParserConstruct            *
 *                               *
 *********************************/
void
ParserConstruct(void)
{
     prsHD = MemAlloc("parser", 1, MAXPWORKSPACE);
     parseHeapDesc = prsHD;
}

/*********************************
 *                               *
 *    ParserDestruct             *
 *                               *
 *********************************/
void          
ParserDestruct(void)
{
     MemDealloc(parseHeapDesc);
}

/*********************************
 *                               *
 *    ParseStream                *
 *                               *
 *********************************/
void
ParseStream(void) {
  int result = 1;

  do {
    if (setjmp(topLevelEnv) == 0) {
      ParserReset();
      yyparse();
      if (!delayedErrorCount)
	      result = ProcessCmd(&ParseResult);
      else
	delayedErrorCount = 0;
    }
    else
      result = 1;
  } while (result);

}


/*********************************
 *                               *
 *    ParserReset                *
 *                               *
 *********************************/
void
ParserReset(void)
{
  MemReset(parseHeapDesc);
  gbChangeScope = BTRUE;
  stPopToTop();       /* pop off all scopes but the last one */
}

/*******************************/
/* Data defs                   */
/*******************************/

/*
 * DATA()
 * Create a new data definition.
 *
 * INPUT:  For a data definition it would be something in the for
 *            data ident1(vars1) -> ident2(vars2) = structors.
 * OUTPUT: new data definition.
 */
void
*DATADEF(char *domainId, STR_LIST *domainVars, 
	 char *codomainId, STR_LIST *codomainVars, 
	 PE_LIST_STRUCTOR *structors)
{
     PE_DATA *data = (PE_DATA *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_DATA));

     assert(domainId || codomainId);
     assert(structors);

     /* [#@]: */

     if (strcmp (domainId,   HASH_NAME) == 0 ||
	 strcmp (codomainId, HASH_NAME) == 0)
       printMsg (DELAYEDERROR_MSG, "illegal use of %s", HASH_NAME);

     if (strcmp (domainId,   AT_NAME) == 0 ||
	 strcmp (codomainId, AT_NAME) == 0)
       printMsg (DELAYEDERROR_MSG, "illegal use of %s", AT_NAME);

     data->domainId     = domainId;
     data->domainVars   = domainVars;    
     data->codomainId   = codomainId;
     data->codomainVars = codomainVars;     
     data->structors    = structors;     

     return(data);
}

/*
 * TYPE()
 * Store the typing info for a data defn internally
 * eg something in the form      
 *      list(A) -> C
 *    store list(A) as a type and is as another type
 *
 * INPUT:  ident = type                 eg: the "list" in list(A)
 *         parms = parameters for type  eg: the "A"    in list(A)
 * OUTPUT: a type 
 */
PE_TYPE  
*TypeNew(char *ident, PE_LIST_TYPE *parms)
{
     PE_TYPE *type = (PE_TYPE *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_TYPE));

     assert(ident);
     assert(type);

     if (strcmp (ident, HASH_NAME) == 0)
       printMsg (DELAYEDERROR_MSG, "illegal use of %s", HASH_NAME);     /* [#@] */

     if (strcmp (ident, AT_NAME) == 0)
       printMsg (DELAYEDERROR_MSG, "illegal use of %s", AT_NAME);

     type->ident = ident;
     type->parms = parms;

     return(type);
}

/*********************************
 *                               *
 *    checkTerminalType          *
 *                               *
 *********************************/
char *
checkTerminalType(char *termType) {

  if (strcmp(termType, TERMINAL_TYPE) != 0) 
    printMsg(DELAYEDERROR_MSG, 
             "Parse - %s is not a valid terminal type", termType);
  else
    return(termType);

}


/*********************************
 *                               *
 *    TypeSigNew                 *
 *                               *
 *********************************/

PE_TYPE_SIG
*TypeSigNew(PE_TYPE *domain, PE_TYPE *codomain)
{
     PE_TYPE_SIG *newtypesig = (PE_TYPE_SIG *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_TYPE_SIG));

     assert(domain);
     assert(codomain);

     newtypesig->domain   = domain;
     newtypesig->codomain = codomain;

     return(newtypesig);
}


/*********************************
 *                               *
 *    TypeSigNew2                *
 *                               *
 *********************************/

/* [H-O] ADDED TO HANDLE TYPE PE_STRUCTOR_TYPE_SIG (SEE parse.h): */

PE_STRUCTOR_TYPE_SIG *
TypeSigNew2 (PE_TYPE *domain,
	     PE_TYPE *param,        /* MAY BE NULL */
	     PE_TYPE *codomain)
{
  PE_STRUCTOR_TYPE_SIG *newSig = NULL;

  assert (domain);
  assert (codomain);

  newSig =
    (PE_STRUCTOR_TYPE_SIG *)MemHeapAlloc (parseHeapDesc,
					  1,
					  sizeof (PE_STRUCTOR_TYPE_SIG));

  assert (newSig);

  newSig->domain   = domain;
  newSig->param    = param;
  newSig->codomain = codomain;

  return (newSig);
}


/*********************************
 *                               *
 *    StructorNew                *
 *                               *
 *********************************/

/* [H-O] ALTERED TO HANDLE TYPE PE_STRUCTOR_TYPE_SIG (SEE parse.h): */

PE_STRUCTOR *
StructorNew (char                 *ident,
	     PE_STRUCTOR_TYPE_SIG *sig)
{
  PE_STRUCTOR *newStructor = NULL;

  assert (ident);
  assert (sig);

  if (strcmp (ident, HASH_NAME) == 0)
    printMsg (DELAYEDERROR_MSG, "illegal use of %s", HASH_NAME);     /* [#@] */

  if (strcmp (ident, AT_NAME) == 0)
    printMsg (DELAYEDERROR_MSG, "illegal use of %s", AT_NAME);

  newStructor = (PE_STRUCTOR *)MemHeapAlloc (parseHeapDesc,
					     1,
					     sizeof (PE_STRUCTOR));

  assert (newStructor);

  newStructor->ident    = ident;
  newStructor->type_sig = sig;

  return newStructor;
}


/*********************************
 *                               *
 *    typeSigof                  *
 *                               *
 *********************************/

/* [H-O] ALTERED TO HANDLE TYPE PE_STRUCTOR_TYPE_SIG (SEE parse.h): */

PE_STRUCTOR_TYPE_SIG *
typeSigof (PE_LIST_STRUCTOR *structorList)
{
  PE_STRUCTOR          *structor          = NULL;
  PE_STRUCTOR_TYPE_SIG *structor_type_sig = NULL;

  assert (structorList);

  structor = StructorListHead (structorList);
  assert (structor);
  structor_type_sig = structor->type_sig;
  assert (structor_type_sig);

  return structor_type_sig;
}


/*********************************
 *                               *
 *    BuildAlias                 *
 *                               *
 *********************************/

PE_ALIAS *
BuildAlias (char     *name,
	    STR_LIST *variables,     /* MAY BE NULL */
	    PE_TYPE  *type)
{
  PE_ALIAS *alias = NULL;

  assert (name);
  assert (type);

  if (strcmp (name, HASH_NAME) == 0)
    printMsg (DELAYEDERROR_MSG, "illegal use of %s", HASH_NAME);     /* [#@] */

  if (strcmp (name, AT_NAME) == 0)
    printMsg (DELAYEDERROR_MSG, "illegal use of %s", AT_NAME);

  alias = (PE_ALIAS *)MemHeapAlloc (parseHeapDesc, 1, sizeof (PE_ALIAS));

  assert (alias);

  alias->name      = name;
  alias->variables = variables;
  alias->type      = type;

  return alias;
}


/****************************************************************************/
/* Def  */


/*********************************
 *                               *
 *    DEFFUNC                    *
 *                               *
 *********************************/
PE_DEF
*DEFFUNC(char *name, PE_LIST_MACRO *macros, PE_TYPE_SIG *type_sig, 
	 PE_DEF *defPart) 
/* def->varbase and def->expr are already filled in */
{
     assert(name);
     assert(defPart);

     if (strcmp (name, HASH_NAME) == 0)
       printMsg (DELAYEDERROR_MSG, "illegal use of %s", HASH_NAME);     /* [#@] */

     if (strcmp (name, AT_NAME) == 0)
       printMsg (DELAYEDERROR_MSG, "illegal use of %s", AT_NAME);

     defPart->id       = name;
     defPart->macros   = macros;
     defPart->type_sig = type_sig;

     return(defPart);
}

/*********************************
 *                               *
 *    pe_MakeFunBody             *
 *                               *
 *********************************/
PE_DEF *
pe_MakeFunBody(PE_LIST_T_PHRASE *t_case) {

     PE_DEF *defPart = (PE_DEF *)MHA(parseHeapDesc, 1, sizeof(PE_DEF));
     PE_T_PHRASE *t_phrase;

     assert(t_case);

     defPart->id       = NULL;
     defPart->macros   = NULL;
     defPart->type_sig = NULL;

     if (T_PhraseListLen(t_case) >= 1)  {
       t_phrase = T_PhraseListHead(t_case);
       if ( ct_isVarBase(t_phrase->patt) && (T_PhraseListLen(t_case) <= 1) ) {
	 defPart->var_base = pe_TranslateVBasePatt(t_phrase->patt);
	 defPart->expr = t_phrase->expr;
       }
       else {
	 defPart->expr   = ExprNew(TermProgNew(t_case), ExprVarNew(RES_VAR_X));
	 defPart->var_base = VBvar(RES_VAR_X);
       }
     }
     else
       printMsg(FATAL_MSG, "pe_MakeFunBody: Illegal function body.");

     return(defPart);

}

/*****************
 *               *
 *   pe_Macros   *
 *               *
 *****************/
PE_LIST_MACRO *
pe_Macros(PE_LIST_MACRO *mlist)
{
  PE_MACRO *macro;
  PE_LIST_MACRO *macros;

  macros = mlist;
  while (macros) {
    macro = MacroListHead(macros);
    macros = MacroListTail(macros);
    if (MacroListMember(macros, macro->ident) == BTRUE) {
      printMsg(DELAYEDERROR_MSG, "Macro %s declared more than once", macro->ident);
    }
    else {
      stAddMacro(macro->ident);
      /* macro->type_sig is not placed in the symbol table until later */
      /* (when parsing a def, we just need to keep track of macro names */
      /*  in a local scope level) */
    }
  }
  return mlist;
}


/*********************************
 *                               *
 *    pe_TranslateVBasePatt      *
 *                               *
 *********************************/
static PE_VAR_BASE *
pe_TranslateVBasePatt(PE_PATT *patt) {

  PE_VAR_BASE  *vb  = (PE_VAR_BASE *)MHA(prsHD, 1, sizeof(PE_VAR_BASE));

  switch (patt->tag) {
  case P_VAR    : 
    vb->tag = VB_VAR;
    vb->info.var = (char *)MHA(prsHD, strlen(patt->info.var) +1, sizeof(char));
    strcpy(vb->info.var, patt->info.var);
    break;
  case P_PAIR   : 
    vb->tag = VB_PAIR;
    vb->info.vbpair.l = pe_TranslateVBasePatt(patt->info.ppair.l);
    vb->info.vbpair.r = pe_TranslateVBasePatt(patt->info.ppair.r);
    break;
  case P_BANG   : 
    vb->tag = VB_BANG;
    break;
  case P_HOVAR :
    printMsg(DELAYEDERROR_MSG, "Higher order variable in function variable base.");
    break;
  default           :
    printMsg(FATAL_MSG,"pe_TranslateVBasePatt - %d is not a valid PE_PATT tag",
	     patt->tag);
  }   /*  hctiws  */
  
  return(vb);

}   /*  end pe_TranslateVBasePatt  */


/* Macros */

/*********************************
 *                               *
 *    MacroNew                   *
 *                               *
 *********************************/
PE_MACRO
*MacroNew(char *ident, PE_TYPE_SIG *type_sig)
{
     PE_MACRO *macro = (PE_MACRO *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_MACRO));

     assert(ident);
     assert(macro);

     if (strcmp (ident, HASH_NAME) == 0)
       printMsg (DELAYEDERROR_MSG, "illegal use of %s", HASH_NAME);     /* [#@] */

     if (strcmp (ident, AT_NAME) == 0)
       printMsg (DELAYEDERROR_MSG, "illegal use of %s", AT_NAME);
     
     macro->ident    = ident;
     macro->type_sig = type_sig;

     return(macro);
}

/* Variable Bases */


/*********************************
 *                               *
 *    VBvar                      *
 *                               *
 *********************************/
PE_VAR_BASE
*VBvar(char *id)
{
     PE_VAR_BASE *var = (PE_VAR_BASE *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_VAR_BASE));

     assert(id);
     assert(var);

     var->tag      = VB_VAR;
     var->info.var = id;

     return(var);
}


/*********************************
 *                               *
 *    VBbang                     *
 *                               *
 *********************************/
PE_VAR_BASE
*VBbang(void)
{
     PE_VAR_BASE *var = 
       (PE_VAR_BASE *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_VAR_BASE));

     var->tag      = VB_BANG;

     return(var);

}


/* Patterns */

/*********************************
 *                               *
 *    peCopyPatt                 *
 *                               *
 *********************************/
PE_PATT *
peCopyPatt(MEMORY heap, PE_PATT *orig) {

    PE_PATT     *result = NULL;
    int          i = 0;

    if ( !orig ) 
        return NULL;

    result = (PE_PATT *)MHA(heap, 1, sizeof(PE_PATT));
    result->tag = orig->tag;

    switch ( orig->tag ) {
    case P_VAR :
        result->info.var = libStrdup(heap, orig->info.var);
        break;
    case P_HOVAR :
        result->info.hovar.hovar = libStrdup(heap, orig->info.hovar.hovar);
        result->info.hovar.destr = orig->info.hovar.destr;
        break;
    case P_PAIR :
        result->info.ppair.l = peCopyPatt(heap, orig->info.ppair.l);
        result->info.ppair.r = peCopyPatt(heap, orig->info.ppair.r);
        break;
    case P_CONSTR :
        result->info.constr = peCopyPStructor(heap, orig->info.constr);
        break;
    case P_RECORD :
        while ( orig->info.record[i++] )     ;
        result->info.record = (P_STRUCTOR **)MHA(heap, i,sizeof(P_STRUCTOR *));
        i = 0;
        while ( orig->info.record[i] ) {
            result->info.record[i] = 
                peCopyPStructor(heap, orig->info.record[i+1]);
            i += 1;
        }
        result->info.record[i] = NULL;
        break;
    case P_BANG :
        break;
    case P_STR : 
        result->info.strBI = libStrdup(heap, orig->info.strBI);
        break;
    case P_CHAR :
    case P_INT :
        result->info.intcharBI.lTag = orig->info.intcharBI.lTag;
        result->info.intcharBI.l = orig->info.intcharBI.l;
        result->info.intcharBI.uTag = orig->info.intcharBI.uTag;
        result->info.intcharBI.u = orig->info.intcharBI.u;
        break;
    default :  
        assert(NULL);
    }   /*  hctiws  */

    return result;

}   /*  end peCopyPatt()  */


/*********************************
 *                               *
 *    peCopyPStructor            *
 *                               *
 *********************************/
P_STRUCTOR *
peCopyPStructor(MEMORY heap, P_STRUCTOR *orig) {
 
    P_STRUCTOR  *result = NULL;

    if ( !orig ) 
        return NULL;

    result = (P_STRUCTOR *)MHA(heap, 1, sizeof(P_STRUCTOR));
    result->id = libStrdup(heap, orig->id);
    result->arg = peCopyPatt(heap, orig->arg);

    return result;

}   /*  end peCopyPStructor()  */


/*********************************
 *                               *
 *    pePatt1                    *
 *                               *
 *********************************/
PE_PATT *
pePatt1(PE_PATT *patty) {

    if ( gbChangeScope ) /* didn't push scope */
        PushScope();   /* keeps things in sync */
    gbChangeScope = BTRUE;

     return patty;

}   /*  end pePatt1()  */


/*********************************
 *                               *
 *    Ppair                      *
 *                               *
 *********************************/
PE_PATT
*Ppair(PE_PATT *l, PE_PATT *r)
/*  sometimes input patterns are null if an error occured earlier */
{
     PE_PATT *pair = (PE_PATT *) MHA(parseHeapDesc, 1, sizeof(PE_PATT));

     assert(pair);

     pair->tag         = P_PAIR;
     pair->info.ppair.l = l;
     pair->info.ppair.r = r;

     return(pair);
}

/* !!!! need to fix with symbol table changes */
/*********************************
 *                               *
 *    Pvar                       *
 *                               *
 *********************************/
PE_PATT *
Pvar(char *id) {

    /* Variables are not allowed to be structors */
  PE_PATT *result = (PE_PATT *)MHA(parseHeapDesc, 1, sizeof(PE_PATT));

  if (strcmp (id, HASH_NAME) == 0)
    printMsg (DELAYEDERROR_MSG, "illegal variable pattern %s", HASH_NAME);     /* [#@] */
  else if (strcmp (id, AT_NAME) == 0)
    printMsg (DELAYEDERROR_MSG, "illegal variable pattern %s", AT_NAME);
  else
    if (isStructor(id)) {
      if (isConstructor(id)) {
	result->tag = P_CONSTR;
	result->info.constr = (P_STRUCTOR *)MHA(prsHD, 1, sizeof(P_STRUCTOR));
	result->info.constr->id = id;
	result->info.constr->arg = (PE_PATT *)MHA(prsHD, 1, sizeof(PE_PATT));
	result->info.constr->arg->tag = P_BANG;
      }   /*  fi  */
      else
	printMsg(DELAYEDERROR_MSG, "%s is a destructor", id);
    }   /*  fi  */
    else {  
      result->tag      = P_VAR;
      result->info.var = peReplaceVar(id, BFALSE);
    }   /*  esle  */

  return result;
}


/*********************************
 *                               *
 *    peHOVar                    *
 *                               *
 *********************************/
PE_PATT *
peHOvar(char *destr, char *var) {
    /* Variables are not allowed to be structors */
    /* lexer has already checked id. We know it's a HO destructor */
  PE_PATT *patt = (PE_PATT *)MHA(parseHeapDesc, 1, sizeof(PE_PATT));

  if (strcmp (var, HASH_NAME) == 0)
    printMsg (DELAYEDERROR_MSG, "illegal variable pattern %s", HASH_NAME);     /* [#@] */
  else if (strcmp (var, AT_NAME) == 0)
    printMsg (DELAYEDERROR_MSG, "illegal variable pattern %s", AT_NAME);
  else
    if ( isStructor(var) )
      printMsg(DELAYEDERROR_MSG,"%s is a structor; should be a variable", var);
    else {  
      patt->tag      = P_HOVAR;
      patt->info.hovar.hovar = peReplaceVar(var, BTRUE);
      patt->info.hovar.destr = st_NameToKey(destr);
    }   /*  esle  */

  return patt;

}   /*  end peHOVar  */


/*********************************
 *                               *
 *    peReplaceVar               *
 *                               *
 *********************************/
static char *
peReplaceVar(char *var, BBOOL isHO) {

    ST_KEY    varKey = st_NameToKey(var);
    int       level = 0;
    BBOOL     failed = BFALSE;

    assert (strcmp (var, HASH_NAME) != 0);     /* [#@] */
    assert (strcmp (var, AT_NAME)   != 0);

    /* Don't worry about reserved variables */
    if ( strncmp(var, RES_PREFIX, strlen(RES_PREFIX)) == 0 ) 
        return var;

    if ( gbChangeScope ) {
        PushScope();
        gbChangeScope = BFALSE;
    }   /*  fi  */

    if ( varKey != NULL ) 
        if ( st_IsVar(varKey, &level) == BTRUE )
            if ( level == 0 ) {
                /* var already in symbol table at this level; nonlinear patt */
                printMsg(DELAYEDERROR_MSG,"%s is a repeated variable", var);
                failed = BTRUE;
            }   /*  fi  */

    if ( !failed )
        /* var is not in symbol table  at this level ---  normal case */
        varKey = stAddVar(var, isHO);

    return libStrdup(prsHD, st_GetUniqueVar(varKey));        

}   /*  end peReplaceVar  */


/*********************************
 *                               *
 *    Pconstr                    *
 *                               *
 *********************************/
PE_PATT *
Pconstr(char *id, PE_PATT *patt)
/* patt may be NULL */
{
     PE_PATT *constr = (PE_PATT *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_PATT));

     if (!delayedErrorCount)
       assert(patt);
     assert(id);
     assert(constr);

     constr->tag         = P_CONSTR;
     constr->info.constr = P_StructorNew(id, patt);

     return(constr);
}

/*********************************
 *                               *
 *    Pdontcare                  *
 *                               *
 *********************************/
PE_PATT
*Pdontcare(void)
{
     PE_PATT *patt = NULL;
     char    *underscore = "_";
     int      len = strlen(underscore) + strlen(RES_PREFIX);

     char *dontcare = (char *) MemHeapAlloc(parseHeapDesc, len+1, sizeof(char));
     strcpy(dontcare, RES_PREFIX);
     strcat(dontcare, underscore);     
     assert(dontcare);

     patt = (PE_PATT *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_PATT));
     assert(patt);
     patt->tag      = P_VAR;
     patt->info.var = dontcare;

     return(patt);
}

/*********************************
 *                               *
 *    Pbang                      *
 *                               *
 *********************************/
PE_PATT
*Pbang(void)
{
     PE_PATT *patt = NULL;

     patt = (PE_PATT *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_PATT));
     assert(patt);

     patt->tag      = P_BANG;

     return(patt);
}


/*********************************
 *                               *
 *    peMakeChar                 *
 *                               *
 *********************************/
char *
peMakeChar(char *str, int radix) {

    char      *c = (char *)MHA(prsHD, 2, sizeof(char));
    long       l = strtol(str, NULL, radix);

    c[0] = c[1] = NULL;
/* !!!! Sun ignores overflow conditions!  Write your own overflow code here */
    if ( (l < CHARMIN)   || (l > CHARMAX) ) 
        printMsg(DELAYEDERROR_MSG, "Illegal character.");
    else
        *c = (char)l;

    return c;

}   /*  end peMakeChar()  */


/*********************************
 *                               *
 *    peMakeInt                  *
 *                               *
 *********************************/
long
peMakeInt(char *intRep) {

    long       l = strtol(intRep, NULL, 10);

/* !!!! Sun ignores overflow conditions!  Write your own overflow code here *
    if ( (l == LONG_MAX) || (l == LONG_MIN) )
        printMsg(DELAYEDERROR_MSG, "Illegal integer.");
*/
    return l;

}   /*  end peMakeInt()  */


/*********************************
 *                               *
 *    Precord                    *
 *                               *
 *********************************/
PE_PATT *Precord(P_STRUCTOR_ARRAY *sArray)
{
  PE_PATT *patt = (PE_PATT *) MHA(parseHeapDesc, 1, sizeof(PE_PATT));
  char **destrNames;
  int i;

  if (!sArray) return NULL;
  destrNames = st_GetStructorNames(sArray->parentKey);

  /* fill in don't care patterns in all unused destructor positions */
  for (i=sArray->numDestructors-1; i>=0; i--) {
    if (!sArray->array[i]) {
      sArray->array[i] = P_StructorNew(destrNames[i], Pdontcare());
    }
  }
  sArray->array[sArray->numDestructors] = 0;	/* null terminate */
  patt->tag         = P_RECORD;
  patt->info.record = sArray->array;
  return patt;
}

/* patterns of a record definition 
 * eg: (head: patt, tail: patt)
 */

/*********************************
 *                               *
 *    P_StructorNew              *
 *                               *
 *********************************/
P_STRUCTOR
*P_StructorNew(char *id, PE_PATT *arg)
/* arg may be NULL */
{
     P_STRUCTOR *structor = (P_STRUCTOR *)MHA(parseHeapDesc, 1, sizeof(P_STRUCTOR));

     if (!delayedErrorCount)
       assert(arg);
     assert(id);
     assert(structor);

     structor->id  = id;
      structor->arg = arg;

     return(structor);
}


/*********************************
 *                               *
 *    P_RecordAdd                *
 *                               *
 *********************************/
P_STRUCTOR_ARRAY *P_RecordAdd(P_STRUCTOR *structor, P_STRUCTOR_ARRAY *sArray)
{
  int structorPosn;

  assert(structor);
  if (!sArray) {
    printMsg(FATAL_MSG, "(parse.c) P_RecordAdd failure on null P_STRUCTOR_ARRAY");
  } else {
    structorPosn = getStructorPosn(structor->id);
    if (sArray->array[structorPosn] != NULL) {
      printMsg(DELAYEDERROR_MSG, "destructor %s occurs more than once in pattern", structor->id);
    }
    else sArray->array[structorPosn] = structor;
    return sArray;
  }
}



/* Terms */

/*********************************
 *                               *
 *    peMakeIntExpr              *
 *                               *
 *********************************/
PE_EXPR *
peMakeIntExpr(long integer) {

    /* intRep is a string of digits representing a number in base 10. *
     * It may have a leading sign indicator.                          */

    PE_TERM     *intTerm = (PE_TERM *)MHA(prsHD, 1, sizeof(PE_TERM));

    intTerm->tag = T_BUILTIN;
    intTerm->info.builtin = peMakeBuiltIn(BI_INT, (void *)&integer);

    return ExprNew(intTerm, ConstNew());

}   /*  peMakeIntExpr()  */


/*********************************
 *                               *
 *    peMakeStrExpr              *
 *                               *
 *********************************/
PE_EXPR *
peMakeStrExpr(char *str) {

    /* str must be stored in the parse heap */
    PE_EXPR *strExpr = ExprNew (TermIdNew ("nil", NULL), ConstNew ());

    if (str)
      {
	int index = 0;

	for (index = strlen (str) - 1; index >= 0; index--)
	  strExpr = ExprNew (TermIdNew ("cons", NULL), ExprPair (peMakeCharExpr (&str[index]), strExpr));
      }

    return strExpr;

}   /*  peMakeStrExpr()  */


/*********************************
 *                               *
 *    peMakeCharExpr             *
 *                               *
 *********************************/
PE_EXPR *
peMakeCharExpr(char *charRep) {

    /* charRep should be a one element string; the element is the char */
    PE_TERM     *charTerm = (PE_TERM *)MHA(prsHD, 1, sizeof(PE_TERM));

    charTerm->tag = T_BUILTIN;
    charTerm->info.builtin = peMakeBuiltIn(BI_CHAR, (void *)&charRep[0]);

    return ExprNew(charTerm, ConstNew());

}   /*  peMakeCharExpr()  */


/*********************************
 *                               *
 *    peMakeBuiltIn              *
 *                               *
 *********************************/
static PE_BUILTIN *
peMakeBuiltIn(PE_BUILTIN_TAG tag, void *data) {

    /* string data must be stored in the parse heap */

    PE_BUILTIN     *result = (PE_BUILTIN *)MHA(prsHD, 1, sizeof(PE_BUILTIN));

    result->tag = tag;

    switch ( tag ) {
    case BI_STRING:
        result->info.strBI = (char *)data;
        break;
    case BI_INT:
        result->info.intBI = *((int *)data);
        break;
    case BI_CHAR:
        result->info.charBI = *((char *)data);
        break;
    default:
        assert(NULL);
    }   /*  hctiws  */

    return result;

}   /*  end peMakeBuiltIn()  */


/*********************************
 *                               *
 *    pe_TermFoldNew             *
 *                               *
 *********************************/
PE_TERM
*pe_TermFoldNew(PE_LIST_FOLD *folds)
{
     PE_TERM           *term         = (PE_TERM *)MHA(prsHD,1,sizeof(PE_TERM));
     PE_FOLD           *fold         = NULL;
     int                numStructs,
                        structPos,
                        numFoldPhrases = 0;
     ST_KEY             sKey,
                        parentKey,
                       *sKeys          = NULL;

     assert(folds);
     assert(term);

     /* get the parent type */
     while (folds) {
       fold = FoldListHead(folds);
       sKey = st_NameToKey(fold->constr);
       if (!sKey)
	 printMsg(DELAYEDERROR_MSG, 
		  "Invalid constructor %s in fold.", fold->constr);
       else if (!st_IsStructor(sKey))
	 printMsg(DELAYEDERROR_MSG,
		  "Invalid constructor %s in fold.", fold->constr);
       else if (!st_IsConstructor(sKey))
	 printMsg(DELAYEDERROR_MSG,
		  "%s is a destructor.", fold->constr);
       else  /* everythings's OK */
	 break;
       folds = FoldListTail(folds);
     }

     if (folds) {
       parentKey    = st_GetStructorParent(sKey);
       sKeys        = st_GetStructorKeys(parentKey);
       numStructs   = st_GetNumStructors(parentKey);
     
       term->tag        = T_FOLD;
       term->info.folds = 
	 (PE_FOLD **) MHA(prsHD, numStructs, sizeof(PE_FOLD *));

       while (folds) {
	 fold = FoldListHead(folds);
	 folds = FoldListTail(folds);

	 /* test to see if constructor is valid eg: of type "parent" */
	 sKey = st_NameToKey(fold->constr);
	 if (!sKey)	
	   printMsg(DELAYEDERROR_MSG,
		    "Invalid constructor %s in fold.", fold->constr);
	 else if (!st_IsStructor(sKey))
	   printMsg(DELAYEDERROR_MSG,
		    "Invalid constructor %s in fold.", fold->constr);
	 else {
	   if (!st_KeysEqual(st_GetStructorParent(sKey), parentKey)) 
	     printMsg(DELAYEDERROR_MSG, "Constructor %s not of type %s",
		      fold->constr, st_KeyToName(parentKey));
	   else {
	     structPos = st_GetStructorPosn(sKey); /* structor posn */

	     /* see if the constructor has already been put into the array */
	     if (term->info.folds[structPos]) 
	       FoldListAppend(term->info.folds[structPos]->phrases, 
			      fold->phrases);
	     else {
	       numFoldPhrases++;
	       term->info.folds[structPos] = fold;
	     }   /*  else  */
	   }   /*  esle  */
	 }   /*  esle  */
       }   /*  elihw  */

       /* check for missing constructors */
       if (numFoldPhrases != numStructs)
	 printMsg(DELAYEDERROR_MSG, 
		  "Should be %d constructors in fold, not %d.",
		  numStructs, numFoldPhrases);
     }   /*  fi  */
     else {
       term->tag        = T_FOLD;
       term->info.folds = NULL;
     }
     return(term);

}

/*********************************
 *                               *
 *    FoldNew                    *
 *                               *
 *********************************/
PE_FOLD
*FoldNew(char *constr, PE_T_PHRASE *phrase)
{
     PE_FOLD *fold = (PE_FOLD *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_FOLD));

/*     assert(constr); */
     assert(phrase);
     assert(fold);

     fold->constr  = constr;
     fold->phrases = T_PhraseListCons(phrase, NULL);

     return(fold);
}

/*********************************
 *                               *
 *    pe_TermUnfoldNew           *
 *                               *
 *********************************/
PE_TERM
*pe_TermUnfoldNew(PE_LIST_UNFOLD *unfolds)
{
  PE_TERM           *term         = (PE_TERM *)MHA(prsHD,1,sizeof(PE_TERM));
  PE_UNFOLD         *unfold       = NULL;
  int                numStructs,
                     structPos,
                     numUnfoldPhrases = 0;
  ST_KEY             sKey,
                     parentKey,
                    *sKeys    = NULL;

  /* get the parent type */
  while (unfolds) {
    unfold = UnfoldListHead(unfolds);
    sKey = st_NameToKey(unfold->destr);
    if (!sKey)
      printMsg(DELAYEDERROR_MSG,
	       "Invalid destructor %s in unfold.", unfold->destr);
    else if (!st_IsStructor(sKey))
      printMsg(DELAYEDERROR_MSG,
	       "Invalid destructor %s in unfold.", unfold->destr);
    else if (!st_IsDestructor(sKey))
      printMsg(DELAYEDERROR_MSG,
	       "%s is a constructor.", unfold->destr);
    else  /* everythings's OK */
      break;
    unfolds = UnfoldListTail(unfolds);
  }

  if (unfolds) {
    parentKey    = st_GetStructorParent(sKey);
    sKeys        = st_GetStructorKeys(parentKey);
    numStructs   = st_GetNumStructors(parentKey);
    
    term->tag          = T_UNFOLD;
    term->info.unfolds = 
      (PE_UNFOLD **)MHA(prsHD, numStructs+1, sizeof(PE_UNFOLD *));
    
    while (unfolds) {
      unfold = UnfoldListHead(unfolds);
      unfolds = UnfoldListTail(unfolds);
      
      /* test to see if destructor is valid eg: of type "parent" */
      sKey = st_NameToKey(unfold->destr);
      if (!sKey)
	printMsg(DELAYEDERROR_MSG,
		 "Invalid destructor %s in unfold.", unfold->destr);
      else if (!st_IsStructor(sKey))
	printMsg(DELAYEDERROR_MSG,
		 "Invalid destructor %s in unfold.", unfold->destr);
      else {
	if (!st_KeysEqual(st_GetStructorParent(sKey), parentKey)) 
	  printMsg(DELAYEDERROR_MSG, "Destructor %s not of type %s",
		   unfold->destr, st_KeyToName(parentKey));
	else {
	  structPos = st_GetStructorPosn(sKey); /* structor posn */
      
	  /* see if the destructor has already been put into the array */
	  if (term->info.unfolds[structPos]) 
	    UnfoldListAppend(term->info.unfolds[structPos]->phrases, 
			     unfold->phrases);
	  else {
	    term->info.unfolds[structPos] = unfold;
	    numUnfoldPhrases++;
	  }   /*  esle  */
	}   /*  esle  */
      }   /*  esle  */
    }   /*  elihw  */
    term->info.unfolds[numStructs] = NULL;   /* null terminated */
    
    /* check for missing destructors */
    if (numUnfoldPhrases != numStructs)
      printMsg(DELAYEDERROR_MSG, 
	       "Should be %d destructors in unfold, not %d.",
	       numStructs, numUnfoldPhrases);
  }   /*  fi  */
  else {
    term->tag        = T_UNFOLD;
    term->info.unfolds = NULL;
  }

  
  return(term);

}

/*********************************
 *                               *
 *    UnfoldNew                  *
 *                               *
 *********************************/
PE_UNFOLD     
*UnfoldNew(char *destr, PE_T_PHRASE *phrase)
{
     PE_UNFOLD *unfold = (PE_UNFOLD *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_UNFOLD));

/*     assert(destr); */
     assert(phrase);
     assert(unfold);

     unfold->destr   = destr;
     unfold->phrases = T_PhraseListCons(phrase, NULL);

     return(unfold);
}

/*********************************
 *                               *
 *    Fold2Unfold                *
 *                               *
 *********************************/
PE_LIST_UNFOLD
*Fold2Unfold(PE_LIST_FOLD *folds)
{
     PE_LIST_UNFOLD *result     = NULL;
     PE_FOLD        *headfold   = NULL;
     PE_UNFOLD      *unfold     = NULL;
     
     if (folds) {
	  headfold = FoldListHead(folds);
	  assert(headfold);

	  unfold = (PE_UNFOLD *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_UNFOLD));
	  unfold->destr   = headfold->constr;
	  unfold->phrases = headfold->phrases;

	  result = UnfoldListCons(unfold, Fold2Unfold(FoldListTail(folds)));
     }

     return(result);
}

/* Fill in the patterns for
 * patt1 => destr1 : expr1
 *       |  destr2 : expr2
 * patt2 => destr1 : expr3
 *       |  destr2 : expr4
 */
/*********************************
 *                               *
 *    UnfoldListAddPatt          *
 *                               *
 *********************************/
PE_LIST_UNFOLD 
*UnfoldListAddPatt(PE_PATT *patt, PE_LIST_UNFOLD *unfolds)
/* patt may be NULL */
{
     PE_LIST_T_PHRASE *listphr    = NULL;
     PE_LIST_UNFOLD   *listunfold = unfolds;
     PE_LIST_UNFOLD   *result     = unfolds;
     PE_UNFOLD        *headunfold = NULL;
     PE_T_PHRASE      *headphr    = NULL;

     assert(unfolds);

     while (listunfold) {

	  headunfold = UnfoldListHead(listunfold);
	  assert(headunfold);
     
	  listphr = headunfold->phrases;
	  assert(listphr);

	  while (listphr) {
	       headphr = T_PhraseListHead(listphr);
	       assert(headphr);
	       if (!headphr->patt)
		    headphr->patt = patt;
	       else
		    break;

	       listphr = T_PhraseListTail(listphr);
	  }

	  listunfold = UnfoldListTail(listunfold);
     }

     return(result);
}


/*********************************
 *                               *
 *    MoreCasesNew               *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION (SEE term.y): */

PE_CASES_AND_UNFOLDS *
MoreCasesNew (PE_T_PHRASE    *newCase,
	      PE_LIST_UNFOLD *unfolds)     /* MAY BE NULL */
{
  PE_CASES_AND_UNFOLDS *casesAndUnfolds =
    (PE_CASES_AND_UNFOLDS *)MemHeapAlloc (parseHeapDesc,
					  1,
					  sizeof (PE_CASES_AND_UNFOLDS));

  assert (newCase);
  assert (casesAndUnfolds);

  casesAndUnfolds->cases   = T_PhraseListCons (newCase, NULL);
  casesAndUnfolds->unfolds = unfolds;

  return casesAndUnfolds;
}


/*********************************
 *                               *
 *    AddMoreCases               *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION (SEE term.y): */

PE_CASES_AND_UNFOLDS *
AddMoreCases (PE_T_PHRASE          *newCase,
	      PE_CASES_AND_UNFOLDS *original)
{
  assert (newCase);
  assert (original);

  original->cases = T_PhraseListCons (newCase, original->cases);

  return original;
}


/*********************************
 *                               *
 *    TermCaseNew                *
 *                               *
 *********************************/
PE_TERM
*TermCaseNew(PE_LIST_T_PHRASE *t_case)
{
 PE_TERM *term = (PE_TERM *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_TERM));

     assert(t_case);
     assert(term);

     term->tag        = T_CASE;
     term->info.cases = t_case;

     return(term);
}

/*********************************
 *                               *
 *    TermProgNew                *
 *                               *
 *********************************/
PE_TERM
*TermProgNew(PE_LIST_T_PHRASE *t_case) {
 PE_TERM *term = (PE_TERM *)MHA(parseHeapDesc, 1, sizeof(PE_TERM));

     term->tag        = T_COMPLETE_CASE;
     term->info.cases = t_case;

     return(term);
}

/*********************************
 *                               *
 *    pe_TermRecordNew           *
 *                               *
 *********************************/
PE_TERM
*pe_TermRecordNew(PE_LIST_RECORD *records)
{
  PE_TERM           *term        = (PE_TERM *)MHA(prsHD,1, sizeof(PE_TERM));
  PE_RECORD         *rec         = NULL;
  int                numStructs,
                     structPos,
                     numRecordPhrases = 0;
  ST_KEY             sKey,
                     parentKey,
                    *sKeys        = NULL;

  /* get the parent type */
  rec = RecordListHead(records);

  sKey = st_NameToKey(rec->destr);
  if (!sKey)
    printMsg(DELAYEDERROR_MSG, 
	     "Invalid destructor %s in record.", rec->destr);
  else {
    parentKey  = st_GetStructorParent(sKey);
    sKeys      = st_GetStructorKeys(parentKey);
    numStructs = st_GetNumStructors(parentKey);
     
    term->tag          = T_RECORD;
    term->info.records = 
      (PE_RECORD **)MHA(prsHD, numStructs, sizeof(struct PE_TERM*));
    while (records) {
      rec = RecordListHead(records);
      records = RecordListTail(records);

      /* test to see if destructor is valid eg: of type "parent" */
      sKey = st_NameToKey(rec->destr);
      if (!sKey)     
	printMsg(DELAYEDERROR_MSG,
		 "Invalid destructor %s in record.", rec->destr);
      else {
	if (!st_KeysEqual(st_GetStructorParent(sKey), parentKey)) 
	  printMsg(DELAYEDERROR_MSG, "Destructor %s not of type %s",
		   rec->destr, st_KeyToName(parentKey));
	else {
	  structPos = st_GetStructorPosn(sKey); /* structor posn */
      
	  /* see if the destructor has already been put into the array */
	  if (term->info.records[structPos]) 
	   printMsg(DELAYEDERROR_MSG,"Duplicate destructor entry %s in record",
		     rec->destr);
	  else {
	    numRecordPhrases++;
	    term->info.records[structPos] = rec;
	  }   /*  esle  */
	}   /*  esle  */
      }   /*  esle  */
    }   /*  elihw  */

    if (numRecordPhrases != numStructs)
      printMsg(DELAYEDERROR_MSG, 
	       "Should be %d destructors in record, not %d.",
	       numStructs, numRecordPhrases);
  }   /*  esle  */

  return(term);

}


/*********************************
 *                               *
 *    RecordNew                  *
 *                               *
 *********************************/

/* [H-O] EXTENDED TO HANDLE H-O PHRASES (SEE parse.h): */

PE_RECORD *
RecordNew (char    *destr,
	   PE_EXPR *expr)
{
  PE_RECORD *record = (PE_RECORD *)MemHeapAlloc (parseHeapDesc,
						 1,
						 sizeof (PE_RECORD));

  assert (destr);
  assert (expr);
  assert (record);

  record->destr = destr;
  record->expr  = expr;
  record->cases = NULL;

  return (record);
}


/*********************************
 *                               *
 *    HORecordNew                *
 *                               *
 *********************************/

/* [H-O] ADDED TO HANDLE H-O PHRASES (SEE parse.h): */

PE_RECORD *
HORecordNew (char    *destr,
	     PE_TERM *cases)
{
  PE_RECORD *record = (PE_RECORD *)MemHeapAlloc (parseHeapDesc,
						 1,
						 sizeof (PE_RECORD));

  assert (destr);
  assert (cases);
  assert (record);

  record->destr = destr;
  record->expr  = NULL;
  record->cases = cases;

  return (record);
}


/*********************************
 *                               *
 *    Phrases2Terms              *
 *                               *
 *********************************/

/*
 * [H-O] ADDED THIS FUNCTION FOR CONVERTING (NECESSARILY UNIVARIANT) LISTS
 *       OF FUNCTION/MACRO MACRO PHRASES TO THEIR FORMS AS EXPECTED BY
 *       TermFunNew() AND TermMacroNew():
 *
 */

static
PE_LIST_TERM *
Phrases2Terms (PE_LIST_FUN_PHRASE *phrases)     /* MAY BE NULL */
{
  if (phrases)
    {
      PE_FUN_PHRASE *head = FunPhraseListHead (phrases);

      if (!head->positive || head->negative)
	{
	  printMsg (DELAYEDERROR_MSG, "macros must be univariant");

	  /* CREATE A DUMMY IF NECESSARY: */

	  if (!head->positive)
	    head->positive = (PE_TERM *)MemHeapAlloc (parseHeapDesc,
						      1,
						      sizeof (PE_TERM));
	}

      return TermListCons (head->positive,
			   Phrases2Terms (FunPhraseListTail (phrases)));
    }
  else
    return NULL;
}


/*********************************
 *                               *
 *    FunPhraseNew               *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION (SEE term.y): */

PE_FUN_PHRASE *
FunPhraseNew (PE_TERM *positive,     /* MAY BE NULL */
	      PE_TERM *negative)     /* MAY BE NULL */
{
  PE_FUN_PHRASE *phrase = (PE_FUN_PHRASE *)MHA(prsHD,1,sizeof (PE_FUN_PHRASE));

  assert (phrase);

  phrase->positive = positive;
  phrase->negative = negative;

  return phrase;
}


/*********************************
 *                               *
 *    TermMacroNew               *
 *                               *
 *********************************/
PE_TERM
*TermMacroNew(char *macroName, PE_LIST_TERM *terms) {

  PE_TERM      *macroTerm = (PE_TERM *)MemHeapAlloc(parseHeapDesc, 1, 
						    sizeof(PE_TERM));
  PE_MACROS    *macro     = (PE_MACROS *)MemHeapAlloc(parseHeapDesc,1,
						      sizeof(PE_MACROS));

  /* can't test for proper number of macros here */
    macro->macro_name = macroName;     
    macro->macros = makePhraseArray(terms, TermListLen(terms));
    macroTerm->tag       = T_MACRO;
    macroTerm->info.macro = macro;

  return(macroTerm);

}   /*  end TermMacroNew  */


/*********************************
 *                               *
 *    TermFunNew                 *
 *                               *
 *********************************/
PE_TERM
*TermFunNew(char *funName, PE_LIST_TERM *terms) {

  PE_TERM      *funTerm = (PE_TERM *)MHA(prsHD, 1, sizeof(PE_TERM));
  PE_FUNCTION  *fun   =(PE_FUNCTION *)MHA(parseHeapDesc,1,sizeof(PE_FUNCTION));

  assert(funName);

  funTerm->tag       = T_FUNCTION;
  funTerm->info.function = fun;

  /* can't tell how many macros since may have been redefined */
  fun->fun_name = funName;     
  fun->macros = makePhraseArray(terms, TermListLen(terms));

  return(funTerm);

}   /*  end TermFunNew  */


/*********************************
 *                               *
 *    makePhraseArray            *
 *                               *
 *********************************/
PE_LIST_T_PHRASE
**makePhraseArray(PE_LIST_TERM *terms, int numElements) {

  PE_LIST_TERM      *tmp    = terms;
  PE_TERM           *term   = NULL;
  PE_LIST_T_PHRASE **phrases = 
    (PE_LIST_T_PHRASE **)MHA(prsHD, numElements+1, sizeof(PE_LIST_T_PHRASE *));
  PE_T_PHRASE      *phrase  = NULL;
  PE_EXPR          *appExpr = NULL;
  int                count  = 0;
  char             *newVar  = NULL;

  for (count = 0; count < numElements; count++) {
    term = TermListHead(tmp);
      
    if (term->tag == T_CASE)
      phrases[count] = term->info.cases;
    else {
      /* make a phrase list with one entry */
      newVar = makeNewRsrvdVar(parseHeapDesc);

      appExpr = (PE_EXPR *)MHA(parseHeapDesc, 1, sizeof(PE_EXPR));
      appExpr->tag = E_VAR;
      appExpr->info.var = newVar;

      phrase =(PE_T_PHRASE *)MHA(parseHeapDesc,1,sizeof(PE_T_PHRASE));
      phrase->patt = Pvar(newVar);
      phrase->expr = ExprNew(term, appExpr);
      
      phrases[count] = T_PhraseListCons(phrase, NULL);
    }   /*  esle  */

    tmp = TermListTail(tmp);
  }   /*  rof  */
  phrases[numElements] = NULL;
  
  return phrases;

}   /*  end makePhraseArray  */


/*********************************
 *                               *
 *    MakeMapPhraseArray         *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION: */

static
PE_MAP_PHRASE *
MakeMapPhraseArray (PE_LIST_FUN_PHRASE *funPhrases,
		    int                 numParams,
		    V_VARIANCE         *varity)
{
  int                 index     = 0;
  PE_LIST_FUN_PHRASE *tmp       = funPhrases;
  PE_FUN_PHRASE      *funPhrase = NULL;
  PE_T_PHRASE        *positive  = NULL;
  PE_T_PHRASE        *negative  = NULL;
  char               *newVar    = NULL;
  PE_EXPR            *appExpr   = NULL;

  PE_MAP_PHRASE *mapPhrases =
    (PE_MAP_PHRASE *)MemHeapAlloc (parseHeapDesc,
				   numParams,
				   sizeof (PE_MAP_PHRASE));

  assert (funPhrases);
  assert (numParams > 0);
  assert (varity);
  assert (mapPhrases);

  for (index = 0; index < numParams; index++)
    {
      funPhrase = FunPhraseListHead (tmp);

      if (funPhrase->positive)
	{
	  if (funPhrase->positive->tag == T_CASE)
	    mapPhrases[index].positive = funPhrase->positive->info.cases;
	  else
	    {
	      newVar = makeNewRsrvdVar (parseHeapDesc);

	      assert (newVar);

	      appExpr = (PE_EXPR *)MemHeapAlloc (parseHeapDesc,
						 1,
						 sizeof (PE_EXPR));

	      assert (appExpr);

	      appExpr->tag      = E_VAR;
	      appExpr->info.var = newVar;

	      positive = (PE_T_PHRASE *)MemHeapAlloc (parseHeapDesc,
						      1,
						      sizeof (PE_T_PHRASE));

	      assert (positive);

	      positive->patt  = Pvar (newVar);
	      positive->expr  = ExprNew (funPhrase->positive, appExpr);
	      positive->cases = NULL;

	      mapPhrases[index].positive = T_PhraseListCons (positive, NULL);
	    }
	}
      else
	mapPhrases[index].positive = NULL;

      if (funPhrase->negative)
	{
	  if (funPhrase->negative->tag == T_CASE)
	    mapPhrases[index].negative = funPhrase->negative->info.cases;
	  else
	    {
	      newVar = makeNewRsrvdVar (parseHeapDesc);

	      assert (newVar);

	      appExpr = (PE_EXPR *)MemHeapAlloc (parseHeapDesc,
						 1,
						 sizeof (PE_EXPR));

	      assert (appExpr);

	      appExpr->tag      = E_VAR;
	      appExpr->info.var = newVar;

	      negative = (PE_T_PHRASE *)MemHeapAlloc (parseHeapDesc,
						      1,
						      sizeof (PE_T_PHRASE));

	      assert (negative);

	      negative->patt  = Pvar (newVar);
	      negative->expr  = ExprNew (funPhrase->negative, appExpr);
	      negative->cases = NULL;

	      mapPhrases[index].negative = T_PhraseListCons (negative, NULL);
	    }
	}
      else
	mapPhrases[index].negative = NULL;

      switch (varity[index])
	{
	case V_NEITHER:
	  if (mapPhrases[index].positive || mapPhrases[index].negative)
	    printMsg (DELAYEDERROR_MSG,
		      "Parameter %d of the map must have \"_\" variance.",
		      index);

	  break;

	case V_POSITIVE:
	  if (!mapPhrases[index].positive || mapPhrases[index].negative)
	    printMsg (DELAYEDERROR_MSG,
		      "Parameter %d of the map must have \"+\" variance.",
		      index);

	  break;

	case V_NEGATIVE:
	  if (!mapPhrases[index].positive || mapPhrases[index].negative)
	    printMsg (DELAYEDERROR_MSG,
		      "Parameter %d of the map must have \"-\" variance.",
		      index);

	  mapPhrases[index].negative = mapPhrases[index].positive;
	  mapPhrases[index].positive = NULL;

	  break;

	case V_BOTH:
	  if (!mapPhrases[index].positive || !mapPhrases[index].negative)
	    printMsg (DELAYEDERROR_MSG,
		      "Parameter %d of the map must have \"*\" variance.",
		      index);

	  break;
	}

      tmp = FunPhraseListTail (tmp);
    }

  return (mapPhrases);
}


/*********************************
 *                               *
 *    TermAliasMapNew            *
 *                               *
 *********************************/

static
PE_TERM *
TermAliasMapNew (char               *alias,
		 PE_LIST_FUN_PHRASE *phrases)
{
  PE_TERM *aliasMapTerm = NULL;
  int      numParams    = 0;

  assert (alias);
  assert (phrases);

  numParams = FunPhraseListLen (phrases);

  if (numParams == getAliasNumParams (alias))
    {
      int                 i          = 0;
      PE_FUN_PHRASE      *tmpPhrase  = NULL;
      PE_LIST_FUN_PHRASE *tmpPhrases = phrases;

      V_VARIANCE *varity    = getAliasVarity    (alias);
      ST_TYPE    *expansion = getAliasExpansion (alias);

      for (i = 0; i < numParams; i++)
	{
	  assert (tmpPhrases);

	  tmpPhrase = FunPhraseListHead (tmpPhrases);

	  switch (varity[i])
	    {
	    case V_NEITHER:
	      if (tmpPhrase->positive || tmpPhrase->negative)
		printMsg (DELAYEDERROR_MSG,
			  "Parameter %d of the map must have \"_\" variance.",
			  i);

	      break;

	    case V_POSITIVE:
	      if (!tmpPhrase->positive || tmpPhrase->negative)
		printMsg (DELAYEDERROR_MSG,
			  "Parameter %d of the map must have \"+\" variance.",
			  i);

	      break;

	    case V_NEGATIVE:
	      if (!tmpPhrase->positive || tmpPhrase->negative)
		printMsg (DELAYEDERROR_MSG,
			  "Parameter %d of the map must have \"-\" variance.",
			  i);

	      tmpPhrase->negative = tmpPhrase->positive;
	      tmpPhrase->positive = NULL;

	      break;

	    case V_BOTH:
	      if (!tmpPhrase->positive || !tmpPhrase->negative)
		printMsg (DELAYEDERROR_MSG,
			  "Parameter %d of the map must have \"*\" variance.",
			  i);

	      break;

	    default:
	      assert (BFALSE);
	    }

	  tmpPhrases = FunPhraseListTail (tmpPhrases);
	}

      assert (!tmpPhrases);

      if (!delayedErrorCount)
	aliasMapTerm = MakeAliasMap (V_POSITIVE, expansion, phrases);
    }
  else
    printMsg (DELAYEDERROR_MSG,
	      "Map for alias %s has invalid number of parameters.", alias);

  if (!aliasMapTerm)     /* FUTURE ASSERTIONS WILL FAIL, SO ... */
    {
      aliasMapTerm = (PE_TERM *)MHA (parseHeapDesc, 1, sizeof (PE_TERM));

      assert (aliasMapTerm);

      aliasMapTerm->tag       = T_MAP;
      aliasMapTerm->info.maps = NULL;
    }

  return aliasMapTerm;
}


/*********************************
 *                               *
 *    MakeAliasMap               *
 *                               *
 *********************************/

static
PE_TERM *
MakeAliasMap (V_VARIANCE          variance,
	      ST_TYPE            *expansion,
	      PE_LIST_FUN_PHRASE *phrases)
{
  PE_TERM *term = (PE_TERM *)MHA (prsHD, 1, sizeof (PE_TERM));

  assert (expansion);
  assert (phrases);
  assert (term);

  switch (expansion->tag)
    {
    case TYPE_1:
      term->tag        = T_CASE;
      term->info.cases = T_PhraseListCons (T_PhraseNew (Pbang (),
							ConstNew ()),
					   NULL);

      break;

    case TYPE_BUILTIN_INT:
      {
	char       *x       = makeNewRsrvdVar   (parseHeapDesc);
	PE_PATT    *patt    = (PE_PATT *)MHA    (parseHeapDesc, 1, sizeof (PE_PATT));
	PE_BUILTIN *builtin = (PE_BUILTIN *)MHA (parseHeapDesc, 1, sizeof (PE_BUILTIN));
	PE_TERM    *BIterm  = (PE_TERM *)MHA    (parseHeapDesc, 1, sizeof (PE_TERM));

	assert (patt);
	assert (builtin);
	assert (BIterm);

	patt->tag                 = P_INT;
	patt->info.intcharBI.lTag = INTX;
	patt->info.intcharBI.l    = (long)1;
	patt->info.intcharBI.uTag = INTX;
	patt->info.intcharBI.u    = (long)1;

	builtin->tag        = BI_INT;
	builtin->info.intBI = (long)1;

	BIterm->tag          = T_BUILTIN;
	BIterm->info.builtin = builtin;

	term->tag        = T_CASE;
	term->info.cases = T_PhraseListCons (T_PhraseNew (Pvar (x),
							  ExprVarNew (x)),
					     T_PhraseListCons (T_PhraseNew (patt,
									    ExprNew (BIterm,
										     ConstNew ())),
							       NULL));
      }

      break;

    case TYPE_BUILTIN_CHAR:
      {
	char       *x       = makeNewRsrvdVar   (parseHeapDesc);
	PE_PATT    *patt    = (PE_PATT *)MHA    (parseHeapDesc, 1, sizeof (PE_PATT));
	PE_BUILTIN *builtin = (PE_BUILTIN *)MHA (parseHeapDesc, 1, sizeof (PE_BUILTIN));
	PE_TERM    *BIterm  = (PE_TERM *)MHA    (parseHeapDesc, 1, sizeof (PE_TERM));

	assert (patt);
	assert (builtin);
	assert (BIterm);

	patt->tag                 = P_CHAR;
	patt->info.intcharBI.lTag = INTX;
	patt->info.intcharBI.l    = (long)65;
	patt->info.intcharBI.uTag = INTX;
	patt->info.intcharBI.u    = (long)65;

	builtin->tag         = BI_CHAR;
	builtin->info.charBI = 'A';

	BIterm->tag          = T_BUILTIN;
	BIterm->info.builtin = builtin;

	term->tag        = T_CASE;
	term->info.cases = T_PhraseListCons (T_PhraseNew (Pvar (x),
							  ExprVarNew (x)),
					     T_PhraseListCons (T_PhraseNew (patt,
									    ExprNew (BIterm,
										     ConstNew ())),
							       NULL));
      }

      break;

    case TYPE_PROD:
      {
	PE_TERM *f = MakeAliasMap (variance, expansion->info.prod.l, phrases);
	PE_TERM *g = MakeAliasMap (variance, expansion->info.prod.r, phrases);

	char *x = makeNewRsrvdVar (parseHeapDesc);
	char *y = makeNewRsrvdVar (parseHeapDesc);

	term->tag        = T_CASE;
	term->info.cases = T_PhraseListCons (T_PhraseNew (Ppair (Pvar (x),
								 Pvar (y)),
							  ExprPair (ExprNew (f,
									     ExprVarNew (x)),
								    ExprNew (g,
									     ExprVarNew (y)))),
					     NULL);
      }

      break;

    case TYPE_USER_DATA:
      {
	int numParams = st_GetNumParams (expansion->info.user_data.key);

	assert (numParams >= 0);

	if (numParams > 0)
	  {
	    PE_MAP     *map     = (PE_MAP *)MHA (parseHeapDesc, 1, sizeof (PE_MAP));
	    V_VARIANCE *varity  = st_GetVarity (expansion->info.user_data.key);
	    int         i       = 0;
	    PE_TERM    *recTerm = NULL;
	    char       *x       = makeNewRsrvdVar (parseHeapDesc);

	    assert (map);
	    assert (numParams ? varity : (V_VARIANCE *)BTRUE);
	    assert (x);

	    map->type_name = st_KeyToName (expansion->info.user_data.key);
	    map->phrases   = (PE_MAP_PHRASE *)MHA (parseHeapDesc,
						   numParams + 1,
						   sizeof (PE_MAP_PHRASE));

	    assert (map->type_name);
	    assert (map->phrases);

	    for (i = 0; i < numParams; i++)
	      {
		assert (expansion->info.user_data.args[i]);
		assert (varity[i]);

		switch (varity[i])
		  {
		  case V_NEITHER:
		    map->phrases[i].positive = NULL;
		    map->phrases[i].negative = NULL;

		    break;

		  case V_POSITIVE:
		    recTerm = MakeAliasMap (variance,
					    expansion->info.user_data.args[i],
					    phrases);

		    map->phrases[i].positive = T_PhraseListCons (T_PhraseNew (Pvar (x),
									      ExprNew (recTerm,
										       ExprVarNew (x))),
								 NULL);
		    map->phrases[i].negative = NULL;

		    break;

		  case V_NEGATIVE:
		    recTerm = MakeAliasMap (Flip (variance),
					    expansion->info.user_data.args[i],
					    phrases);

		    map->phrases[i].positive = NULL;
		    map->phrases[i].negative = T_PhraseListCons (T_PhraseNew (Pvar (x),
									      ExprNew (recTerm,
										       ExprVarNew (x))),
								 NULL);

		    break;

		  case V_BOTH:
		    recTerm = MakeAliasMap (variance,
					    expansion->info.user_data.args[i],
					    phrases);

		    map->phrases[i].positive = T_PhraseListCons (T_PhraseNew (Pvar (x),
									      ExprNew (recTerm,
										       ExprVarNew (x))),
								 NULL);

		    recTerm = MakeAliasMap (Flip (variance),
					    expansion->info.user_data.args[i],
					    phrases);

		    map->phrases[i].negative = T_PhraseListCons (T_PhraseNew (Pvar (x),
									      ExprNew (recTerm,
										       ExprVarNew (x))),
								 NULL);

		    break;

		  default:
		    assert (BFALSE);
		  }
	      }

	    map->phrases[i].positive = NULL;
	    map->phrases[i].negative = NULL;

	    term->tag       = T_MAP;
	    term->info.maps = map;
	  }
	else
	  {
	    char *x = makeNewRsrvdVar (parseHeapDesc);

	    term->tag        = T_CASE;
	    term->info.cases = T_PhraseListCons (T_PhraseNew (Pvar (x),
							      ExprVarNew (x)),
						 NULL);
	  }
      }

      break;

    case TYPE_PARAMETRIC_VAR:
      {
	PE_FUN_PHRASE *phrase = FunPhraseListIndex (phrases, (int)expansion->info.param_var);

	assert (phrase);

	switch (variance)
	  {
	  case V_POSITIVE:
	    term = phrase->positive;
	    break;

	  case V_NEGATIVE:
	    term = phrase->negative;
	    break;

	  default:
	    assert (BFALSE);
	  }
      }

      break;

    default:
      assert (BFALSE);
    }

  assert (term);

  return term;
}


/*********************************
 *                               *
 *    TermMapNew                 *
 *                               *
 *********************************/

/*
 * [H-O] ALTERED THIS FUNCTION TO HANDLE MULTIVARIANT MAP PHRASES (SEE term.y):
 *
 */

static
PE_TERM *
TermMapNew (char               *type,
	    PE_LIST_FUN_PHRASE *phrases)
{
  PE_TERM *mapTerm   = (PE_TERM *)MHA (prsHD, 1, sizeof (PE_TERM));
  PE_MAP  *map       = (PE_MAP  *)MHA (prsHD, 1, sizeof (PE_MAP));
  int      numParams = 0;

  assert (type);
  if (!delayedErrorCount) assert(phrases);	/* might be NULL */
  assert (mapTerm);
  assert (map);

  numParams = FunPhraseListLen (phrases);

  if (numParams == getNumParams (type))
    {
      map->type_name = type;
      map->phrases   = MakeMapPhraseArray (phrases,
					   numParams,
					   st_GetVarity (st_NameToKey (type)));

      mapTerm->tag       = T_MAP;
      mapTerm->info.maps = map;
    }
  else
    printMsg (DELAYEDERROR_MSG,
	      "Map for datatype %s has invalid number of parameters.", type);

/*
  numParams = getNumParams(type);
  if (numParams == TermListLen(terms)) {
    map->type_name = type;     
    map->phrases = makePhraseArray(terms, numParams);
    mapTerm->tag       = T_MAP;
    mapTerm->info.maps = map;
  }
  else 
    printMsg(DELAYEDERROR_MSG, 
	     "Map for datatype %s has invalid number of parameters.", type);
*/

  return mapTerm;
}


/*********************************
 *                               *
 *    TermStructorNew            *
 *                               *
 *********************************/
PE_TERM
*TermStructorNew(char *structor)   {
  
  PE_TERM *structorTerm = (PE_TERM *)MemHeapAlloc(parseHeapDesc, 1, 
						  sizeof(PE_TERM));
  assert(structor);

  structorTerm->tag = T_STRUCTOR;
  structorTerm->info.struct_name = structor;

  return(structorTerm);

}   /*  end TermStructorNew  */


/*********************************
 *                               *
 *    TermIdNew                  *
 *                               *
 *********************************/
PE_TERM *
TermIdNew(char *id, PE_LIST_FUN_PHRASE *phrases) {
    /* Variables can shadow anything */
    ST_KEY    idKey = st_NameToKey(id);

    if (idKey == 0) {
      printMsg(DELAYEDERROR_MSG,"unknown identifier \"%s\"", id);
      return TermMacroNew(id, NULL);	/* return something arbitrary (not NULL)
					   so subsequent assertions don't fail... */
    }

    else if (st_IsVar(idKey, NULL) == BTRUE) {
      /* should be a h.o. variable with no macro arguments */
      if (phrases) {
	printMsg(DELAYEDERROR_MSG, "inappropriate use of variable \"%s\"", id);
      }
      else if (st_IsHOVar(idKey, NULL) == BFALSE) {
	printMsg(DELAYEDERROR_MSG, "inappropriate use of first-order variable \"%s\"", id);
      }
/*
      return TermFunNew(libStrdup(prsHD,st_GetUniqueVar(idKey)),Phrases2Terms(phrases));
*/

      return TermFunNew(libStrdup(prsHD,st_GetUniqueVar(idKey)), NULL);
    }

    else if (st_IsMacro(idKey)) {
      if (phrases) {
	printMsg(DELAYEDERROR_MSG, "inappropriate use of macro \"%s\"", id);
      }
      return TermMacroNew(id, NULL);
    }

    else if (st_IsFunction(idKey)) {
      if (phrases && strcmp (id, AT_NAME) == 0)
	printMsg (DELAYEDERROR_MSG, "inappropriate use of %s", AT_NAME);     /* [#@] */

      return TermFunNew(id, Phrases2Terms(phrases));
    }

    else if (st_IsDatatype(idKey)) {
      if (!phrases) {
	printMsg(DELAYEDERROR_MSG, "inappropriate use of datatype identifier \"%s\"", id);
	return TermMacroNew(id, NULL);	/* return something arbitrary (not NULL)
					   so subsequent assertions won't fail */
      }
      else return TermMapNew(id, phrases);
    }

    else if (isAlias (id))
      {
	if (!phrases)
	  {
	    printMsg (DELAYEDERROR_MSG,
		      "inappropriate use of type alias \"%s\"",
		      id);

	    return TermMacroNew (id, NULL);
	  }

	return TermAliasMapNew (id, phrases);
      }

    else if (st_IsStructor(idKey)) {
      if (phrases) {
	printMsg(DELAYEDERROR_MSG, "inappropriate use of structor \"%s\"", id);
      }
      return TermStructorNew(id);
    }

    printMsg(FATAL_MSG,"TermIdNew: unknown identifier \"%s\"", id);

}   /*  end TermIdNew()  */


/*********************************
 *                               *
 *    T_PhraseNew                *
 *                               *
 *********************************/

/* [H-O] EXTENDED TO HANLDE H-O PHRASES (SEE parse.h): */

PE_T_PHRASE *
T_PhraseNew (PE_PATT *patt,     /* MAY BE NULL */
	     PE_EXPR *expr)
{
  PE_T_PHRASE *phrase = (PE_T_PHRASE *)MemHeapAlloc (parseHeapDesc,
						     1,
						     sizeof (PE_T_PHRASE));

  assert (expr);
  assert (phrase);

  phrase->patt  = patt;
  phrase->expr  = expr;
  phrase->cases = NULL;

  return (phrase);
}


/*********************************
 *                               *
 *    HOT_PhraseNew              *
 *                               *
 *********************************/

/* [H-O] ADDED TO HANDLE H-O PHRASES (SEE parse.h): */

PE_T_PHRASE *
HOT_PhraseNew (PE_PATT *patt,      /* MAY BE NULL */
	       PE_TERM *cases)
{
  PE_T_PHRASE *phrase = (PE_T_PHRASE *)MemHeapAlloc (parseHeapDesc,
						     1,
						     sizeof (PE_T_PHRASE));

  assert (cases);
  assert (phrase);

  phrase->patt  = patt;
  phrase->expr  = NULL;
  phrase->cases = cases;

  return (phrase);
}


/* Fill in the patterns for
 *   nil:   patt1 => expr1
 * | cons:  patt2 => expr2
 *       |  patt3 => expr3
 */
/*********************************
 *                               *
 *    FoldListAddId              *
 *                               *
 *********************************/
PE_LIST_FOLD
*FoldListAddId(char *id, PE_LIST_FOLD *folds)
{
     PE_LIST_FOLD  *result    = folds;
     PE_LIST_FOLD  *listfold  = folds;
     PE_FOLD       *headfold  = NULL;

     assert(id);
     assert(folds);

     while (listfold) {
	  headfold = FoldListHead(listfold);
	  assert(headfold);

	  if (!headfold->constr)
	       headfold->constr = id;
	  else
	       break;
	  listfold = FoldListTail(listfold);
     }

     return(result);
}


/* Expressions */

/*********************************
 *                               *
 *    ExprIdNew                  *
 *                               *
 *********************************/
PE_EXPR
*ExprIdNew(char *id)   {
    /* Variables can shadow anything */

    PE_EXPR *nexpr = NULL;
    ST_KEY    idKey = st_NameToKey(id);

    if (idKey == 0) {
	printMsg(DELAYEDERROR_MSG, "unknown identifier \"%s\"", id);
        nexpr = ExprNew(TermMacroNew(id, NULL), ConstNew());
	/* nexpr is arbitrary (but not NULL) so subsequent assertions won't fail */
    }
    else if ((st_IsVar(idKey,NULL) == BTRUE) && (st_IsHOVar(idKey,NULL) == BFALSE)) {
        /* Insert the variable's unique alias */
        nexpr = (PE_EXPR *)MHA(prsHD, 1, sizeof(PE_EXPR));
        nexpr->tag = E_VAR;

	if (strcmp (id, HASH_NAME) == 0)     /* [#@] */
	  nexpr->info.var = id;
	else
	  nexpr->info.var = libStrdup (prsHD, st_GetUniqueVar (idKey));
      }
    else         /* it should be a structor, function or macro */
        nexpr = ExprNew(TermIdNew(id, NULL), ConstNew());

    return nexpr;

}

/*********************************
 *                               *
 *    ExprVarNew                 *
 *                               *
 *********************************/
PE_EXPR
*ExprVarNew(char *var)  {

  PE_EXPR *nexpr = (PE_EXPR *) MemHeapAlloc(prsHD, 1, sizeof(PE_EXPR));
  nexpr->tag = E_VAR;
  nexpr->info.var = var;

  return(nexpr);

}

/*********************************
 *                               *
 *    ExprNew                    *
 *                               *
 *********************************/
PE_EXPR
*ExprNew(PE_TERM *term, PE_EXPR *expr) {
     PE_EXPR *nexpr = (PE_EXPR *) MHA(parseHeapDesc, 1, sizeof(PE_EXPR));

     assert(term);     assert(expr);     assert(nexpr);

     nexpr->tag           = E_APP;
     nexpr->info.app.term = term;
     nexpr->info.app.expr = expr;

     return nexpr;     
}

/*********************************
 *                               *
 *    ExprPair                   *
 *                               *
 *********************************/
PE_EXPR
*ExprPair(PE_EXPR *l, PE_EXPR *r)
{
     PE_EXPR *nexpr = (PE_EXPR *) MemHeapAlloc(parseHeapDesc, 1, sizeof(PE_EXPR));

     assert(l);
     assert(r);
     assert(nexpr);

     nexpr->tag          = E_PAIR;
     nexpr->info.epair.l = l;
     nexpr->info.epair.r = r;

     return(nexpr);     
}

/*********************************
 *                               *
 *    ConstNew                   *
 *                               *
 *********************************/
PE_EXPR *
ConstNew(void) {
     PE_EXPR *nexpr = (PE_EXPR *)MHA(parseHeapDesc, 1, sizeof(PE_EXPR));
     assert(nexpr);

     nexpr->tag  = E_BANG;
     return nexpr;  
}


/*********************************
 *                               *
 *    peMakeGuardExpr            *
 *                               *
 *********************************/
PE_EXPR *
peMakeGuardExpr(PE_EXPR *term, PE_LIST_LIST_T_PHRASE *casesList) {
/* term cannot be NULL */

  PE_EXPR           *result = term;
  PE_LIST_T_PHRASE  *cases = NULL;

  while ( casesList ) {
      cases = T_PhraseListListHead(casesList);
      casesList = T_PhraseListListTail(casesList);
      result = ExprNew(TermProgNew(cases), result);
  }   /*  elihw  */

  return result;

}   /*  end peMakeGuardExpr()  */


/*********************************
 *                               *
 *    peMakeGuardBool            *
 *                               *
 *********************************/
PE_EXPR *
peMakeGuardBool(PE_EXPR *trueCase, PE_EXPR *falseCase, PE_EXPR *cond) {


    PE_PATT           *truePatt = Pvar("true");
    PE_PATT           *falsePatt = Pvar("false");
    PE_LIST_T_PHRASE  *phrases = NULL;

    if ( falseCase != NULL )
        phrases = T_PhraseListCons(T_PhraseNew(falsePatt, falseCase), NULL);

    phrases = T_PhraseListCons(T_PhraseNew(truePatt, trueCase), phrases);

    return ExprNew(TermProgNew(phrases), cond);

}   /*  end peMakeGuardBool()  */


/*********************************
 *                               *
 *    pe_MakePattNilList         *
 *                               *
 *********************************/
PE_PATT *
pe_MakePattNilList(void) {

  return(Pconstr("nil", Pbang()));

}   /*  end pe_MakePattNilList  */


/*********************************
 *                               *
 *    pe_MakePattList            *
 *                               *
 *********************************/
PE_PATT *
pe_MakePattList(PE_PATT *head, PE_PATT *tail) {
/* head may be NULL */

    if (tail) 
      return(Pconstr("cons", Ppair(head, tail)));
    else
      return(Pconstr("cons", Ppair(head, Pconstr("nil", Pbang()))));

}   /*  pe_MakePattList  */


/*********************************
 *                               *
 *    pe_MakeIntPatt             *
 *                               *
 *********************************/
PE_PATT *
pe_MakeIntPatt(PE_INT_TAG tag1, long i1, PE_INT_TAG tag2, long i2) {

    PE_PATT  *result = NULL;

    if ( (tag1 == INTX) && (tag2 == INTX) && (i1 > i2) ) {
        printMsg(DELAYEDERROR_MSG, 
          " In integer pattern, %d must be less than or equal to %d.", i1, i2);
        return NULL;
    }   /*  fi  */

    result = (PE_PATT *)MHA(prsHD, 1, sizeof(PE_PATT));
    result->tag = P_INT;
    result->info.intcharBI.lTag = tag1;
    result->info.intcharBI.l = i1;
    result->info.intcharBI.uTag = tag2;
    result->info.intcharBI.u = i2;

    return result;

}   /*  end pe_MakePattInteger  */


/*********************************
 *                               *
 *    peMakeCharPatt             *
 *                               *
 *********************************/
PE_PATT *
peMakeCharPatt(char *cl, char *cu) {

/* The lowest possible char is called NEGINF and the highest
 * possible char is called POSINF. This is so that the pattern
 * match translater can check for complete patterns.
 */

    PE_PATT  *result = NULL;

    if ( cl[0] > cu[0] ) {
        printMsg(DELAYEDERROR_MSG, " In char pattern, %s must succeed %s.", cu, cl);
        return NULL;
    }   /*  fi  */

    result = (PE_PATT *)MHA(prsHD, 1, sizeof(PE_PATT));
    result->tag = P_CHAR;
    if ( (long)cl[0] == CHARMIN )
        result->info.intcharBI.lTag = NEGINF;
    else
        result->info.intcharBI.lTag = INTX;
    result->info.intcharBI.l = (long)cl[0];

    if ( (long)cu[0] == CHARMAX )
        result->info.intcharBI.uTag = POSINF;
    else
        result->info.intcharBI.uTag = INTX;
    result->info.intcharBI.u = (long)cu[0];

    return result;

}   /*  end peMakeCharPatt  */


/*********************************
 *                               *
 *    pe_MakeStrPatt             *
 *                               *
 *********************************/
PE_PATT *
pe_MakeStrPatt(char *str) {

    PE_PATT *result = NULL;

    if (!str || *str == '\0')
      result = pe_MakePattNilList ();
    else
      {
	int index = strlen (str) - 1;

	result = pe_MakePattList (peMakeCharPatt (&str[index], &str[index]), NULL);

	for (--index; index >= 0; index--)
	  result = pe_MakePattList (peMakeCharPatt (&str[index], &str[index]), result);
      }

    return result;

}   /*  end pe_MakeStrPatt  */


/*********************************
 *                               *
 *    pe_MakePattDestr           *
 *                               *
 *********************************/
P_STRUCTOR_ARRAY *pe_MakePattDestr(char *id, PE_PATT *patt, P_STRUCTOR_ARRAY *sArray)
{
  ST_KEY idKey = st_NameToKey(id);
  P_STRUCTOR_ARRAY *new_sArray;
  int numDestrs;

  if (idKey) {
    if (st_IsStructor(idKey)) {
      if (st_IsDestructor(idKey)) {
	if (!sArray) {
	  new_sArray = (P_STRUCTOR_ARRAY *) MHA(parseHeapDesc, 1, sizeof(P_STRUCTOR_ARRAY));
	  new_sArray->parentKey = st_GetStructorParent(idKey);
	  numDestrs = st_GetNumStructors(new_sArray->parentKey);
	  new_sArray->numDestructors = numDestrs;
	  new_sArray->array = (P_STRUCTOR **) MHA(parseHeapDesc, numDestrs+1, sizeof(P_STRUCTOR *));
          new_sArray->array[numDestrs] = NULL;
	  return P_RecordAdd(P_StructorNew(id, patt), new_sArray);
	}
	else if (st_GetStructorParent(idKey) == sArray->parentKey) {
	  return P_RecordAdd(P_StructorNew(id, patt), sArray);
	}
	else {
	  printMsg(DELAYEDERROR_MSG, "incompatible destructors in pattern");
	  return NULL;
	}
      }
    }
  }
  printMsg(DELAYEDERROR_MSG, "expected %s to be a destructor (in pattern)", id);
  return NULL;
}



/*********************************
 *                               *
 *    pe_MakePattConstr          *
 *                               *
 *********************************/
PE_PATT *
pe_MakePattConstr(char *id, PE_PATT *patt) {

  ST_KEY idKey = st_NameToKey(id);

  if (idKey) 
    if (st_IsStructor(idKey)) 
      if (st_IsConstructor(idKey)) 
	return(Pconstr(id, patt));

  printMsg(DELAYEDERROR_MSG, "expected %s to be a constructor (in pattern)", id);
  return(NULL);

}   /*  end pe_MakePattConstr  */


/************************************
 *                                  *
 *    peMakeVarPatt                 *
 *                                  *
 ************************************/
PE_PATT *
peMakeVarPatt(MEMORY heap, char *var, BBOOL copyVar) {

  PE_PATT *patt = (PE_PATT *)MHA(heap, 1, sizeof(PE_PATT));
  patt->tag = P_VAR;

  if ( copyVar )
      patt->info.var = libStrdup(heap, var);
  else
     patt->info.var = var;

  return patt;

}


/************************************
 *                                  *
 *    EnterFold                     *
 *                                  *
 ************************************/

/* [#@] */

void
EnterFold (void)
{
  PushScope ();
  stAddVar (HASH_NAME, BFALSE);
}


/************************************
 *                                  *
 *    ExitFold                      *
 *                                  *
 ************************************/

/* [#@] */

void
ExitFold (void)
{
  PopScope ();
}


/************************************
 *                                  *
 *    EnterUnfold                   *
 *                                  *
 ************************************/

/* [#@] */

void
EnterUnfold (void)
{
  PE_DEF def;

  def.id       = AT_NAME;
  def.macros   = NULL;
  def.type_sig = NULL;
  def.var_base = NULL;
  def.expr     = NULL;

  PushScope ();
  st_AddFunction (&def);
}


/************************************
 *                                  *
 *    ExitUnfold                    *
 *                                  *
 ************************************/

/* [#@] */

void
ExitUnfold (void)
{
  PopScope ();
}

