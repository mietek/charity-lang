/******************************************************************************
 *                                                                            *
 *   addSymtab.c                                                              *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <assert.h>
#include <string.h>

#include "symtab.h"
#include "symtabI.h"
#include "ioChar.h"
#include "lib.h"
#include "list.h"
#include "listParse.h"
#include "variance.h"      /* [H-O] ADDED THIS INCLUDE */
#include "machine.h"       /* [BI]  ADDED THIS INCLUDE */

BBOOL  gb_ReplaceFunctions = BFALSE;
MEMORY scratchHD;                        /* must be created & cleaned up */

/*****************************************************************************
 *                                                                           *
 *            Prototypes For Internal Functions                              *
 *                                                                           *
 *****************************************************************************/

/* prototypes */
static BBOOL      existsStateVar(char *domainId,
                                  STR_LIST *domainVars,
                                  char *codomainId,
                                  STR_LIST *codomainVars,
                                  PE_LIST_STRUCTOR *structors,
                                  ST_DT_KIND kind);

/* [H-O] ADDED THE LAST PARAMETER (SEE BELOW): */

static BBOOL      areValidStructDefns(PE_LIST_STRUCTOR *structors,
                                      char             *name,
                                      STR_LIST         *paramList,
                                      char             *stateVar,
                                      BBOOL             nameClash,
                                      ST_DT_KIND        class);

/* [H-O] ADDED THE LAST PARAMETER (SEE BELOW): */

static void       domainOK (PE_TYPE  *domain,
                            STR_LIST *paramList,
                            char     *stateVar,
                            char     *main,
                            char     *structName,
                            char     *name,
                            BBOOL     allowStateVar);

static void       putTypeSig(ST_TYPE *main,
                              PE_TYPE *peMain,
                              STR_LIST *paramList);

/* [H-O] ADDED THESE FUNCTIONS (SEE BELOW): */

static BBOOL      VarCheck  (ST_ENTRY *typeEntry);
static void       CheckType (V_VARIANCE  v,
                             ST_TYPE    *type,
                             V_VARIANCE *varity,
                             V_VARIANCE *stateVariance);

/* [H-O] ALTERED THE TYPE OF THE SECOND PARAMETER (SEE parse.h): */

static void       addTypeSigToEntry (ST_TYPE_SIG          *typeSig,
                                     PE_STRUCTOR_TYPE_SIG *peTypeSig,
                                     STR_LIST             *paramList);

static void       putStructorEntry(PE_STRUCTOR *structor,
                                    STR_LIST *paramList,
                                    int i,
                                    ST_ENTRY *parent,
                                   ST_KEY key);
static ST_ENTRY  *st_MakeOpComb(char *combName, char *name, ST_ENTRY *parent, int posn);
static ST_TYPE_SIG *st_MakeCaseTypeSig(ST_ENTRY *combName,
                                     ST_KEY *structKeys,
                                     int numStructs);
static ST_TYPE_SIG *st_MakeFoldTypeSig(ST_ENTRY *combEntry,
                                       ST_KEY *structKeys,
                                       int numStructs);

/* [H-O] ADDED THE THIRD PARAMETER TO THE PROTOTYPE (SEE BELOW): */

static ST_TYPE_SIG *st_MakeMapTypeSig (ST_ENTRY   *combEntry,
                                       int         numParams,
                                       V_VARIANCE *varity);

static ST_TYPE_SIG *st_MakeUnfoldTypeSig(ST_ENTRY *combEntry,
                                         ST_KEY *structKeys,
                                         int numStructs);
static ST_TYPE_SIG *st_MakeRecordTypeSig(ST_ENTRY *combEntry,
                                         ST_KEY *structKeys,
                                         int numStructs);
static ST_TYPE_SIG *st_MakeGenCombTypeSig(ST_KEY *structKeys,
                                          int numStructs,
                                          ST_TYPE *rplcmnt,
                                          ST_PVAR env,
                                          ST_PVAR cod);

static BBOOL      st_ContainsStateVar(ST_TYPE *type);
static ST_TYPE   *st_ReplaceStateVar(ST_TYPE *type, ST_TYPE *rplcmnt);
static ST_TYPE   *st_ChangeParamVars(ST_TYPE *type, int numParams);

static BBOOL       st_IsNameClash(char *name);
static BBOOL       st_HandleNameClash(ST_ENTRY *entry, char *kind);

static ST_DT_KIND     checkDatatype(PE_DATA *dataDefn);
static ST_ENTRY *buildTypeEntry (PE_DATA *dataDefn,
                                      ST_DT_KIND class,
                                      char *name,
                                      STR_LIST *paramList);

static ST_TYPE      *st_MakeProdST_Type(ST_TYPE *left, ST_TYPE *right);

static ST_TYPE          *st_CopyType(ST_TYPE *type);
static ST_TYPE_SIG      *st_CopyTypeSig(ST_TYPE_SIG *sig, int numParams);

static ST_KEY        st_AddMacro(ST_KEY funKey,
                                 PE_MACRO *macro,
                                 ST_TYPE_SIG *mSig,
                                 int i);

static ST_KEY        _st_AddFunction(PE_DEF *def);

static ST_TYPE_SIG *st_DefSigToEntrySig(PE_TYPE_SIG *defSig,
                                        PE_LIST_MACRO *macros);
static ST_TYPE_SIG *st_PESigToEntrySig(PE_TYPE_SIG *PESig,
                                       STR_LIST **pNameList);
static ST_TYPE     *st_PETypeToEntryType(PE_TYPE *PEMain,
                                         STR_LIST **pNameList);
static ST_TYPE     *st_HandleDatatype(PE_LIST_TYPE *parms,
                                      ST_ENTRY *entry,
                                      STR_LIST **pNameList);

static ST_TYPE *st_HandleParamType (char      *ident,
                                    STR_LIST **pNameList);

static ST_TYPE_SIG *st_NullSigToEntrySig(ST_PVAR *lastParam);
static void         st_TranslateCleanup(void);


/*
 *  TYPE ALIAS RELATED FUNCTIONS
 *
 */

static void     CheckBindings          (STR_LIST      *variables,
                                        PE_TYPE       *type);
static ST_TYPE *ExpandAlias            (PE_TYPE       *alias,
                                        ST_ENTRY      *aliasEntry,
                                        STR_LIST     **pNameList);
static void     ExpansionSubstitution  (PE_TYPE       *alias,
                                        ST_TYPE      **expansion,
                                        int            numParams,
                                        STR_LIST     **pNameList);
static void     ExpansionSubstitution2 (ST_LIST_TYPE  *types,
                                        ST_TYPE      **expansion,
                                        int            numParams);


static ST_LIST_TYPE *StTypeListCons  (ST_TYPE      *x,
                                      ST_LIST_TYPE *xs,
                                      MEMORY        hd);
static ST_TYPE      *StTypeListHead  (ST_LIST_TYPE *xs);
static ST_LIST_TYPE *StTypeListTail  (ST_LIST_TYPE *xs);
static ST_TYPE      *StTypeListIndex (ST_LIST_TYPE *xs,
                                      int           index);

/*********************************
 *                               *
 *    existsStateVar             *
 *                               *
 *********************************/
static BBOOL
existsStateVar(char *domainId,
               STR_LIST *domainVars,
               char *codomainId,
               STR_LIST *codomainVars,
               PE_LIST_STRUCTOR *structors,
               ST_DT_KIND kind) {

  PE_STRUCTOR *structor;

  /* domainId is the potential stateVar */
  if (domainVars)         /* domainVars should be empty */
    return(BFALSE);
  if (strcmp(domainId, codomainId) == 0)  /* stateVar != codomainId */
    return(BFALSE);
  if (StrListMember(codomainVars, domainId)) /* stateVar can't be a param */
    return(BFALSE);

  do {
    structor = StructorListHead(structors);
    if (kind == DT_INDUCTIVE) {
      if (strcmp(structor->type_sig->codomain->ident, domainId) != 0)
        return(BFALSE);
      if (structor->type_sig->codomain->parms)  /* stateVar takes no parms */
        return(BFALSE);
    }   /*  fi  */
    else {
      if (strcmp(structor->type_sig->domain->ident, domainId) != 0)
        return(BFALSE);
      if (structor->type_sig->domain->parms)  /* stateVar takes no parms */
        return(BFALSE);
    }   /*  esle  */
  } while ( (structors = StructorListTail(structors)) );

  return(BTRUE);

}   /*  end existsStateVar  */


/*********************************
 *                               *
 *    areValidStructDefns        *
 *                               *
 *********************************/

BBOOL
areValidStructDefns (PE_LIST_STRUCTOR *structors,
                     char             *name,
                     STR_LIST         *paramList,
                     char             *stateVar,
                     BBOOL             nameClash,
                     ST_DT_KIND        class)     /* [H-O] ADDED (SEE BELOW) */
{
  PE_STRUCTOR *structor;
  STR_LIST    *structorNames = NULL;

  delayedErrorCount = 0;

  scratchHD = MemAlloc("symtab scratch1", 100, sizeof(char *));
  do {
    structor = StructorListHead(structors);
    if (strcmp(name, structor->ident) == 0)  /* must not be type name */
      printMsg(DELAYEDERROR_MSG,
               "Datatype: %s - Structor name same as parent datatype name",
               structor->ident);
    if (StrListMember(structorNames, structor->ident))
      /* structor name can't be repeated */
      printMsg(DELAYEDERROR_MSG,
               "Datatype: %s - Structor name is repeated (%s).",
               name,structor->ident);
    structorNames = StrListCons(structor->ident, structorNames, scratchHD);

    if (StrListMember(paramList, structor->ident))
      /* structor name shouldn't be a parameter name */
      printMsg(WARN_MSG,
              "Datatype: %s - Structor name \"%s\" is a parameter identifier",
               name,structor->ident);

    /*
     * [FIXED] ADDED A SECOND STRUCTOR NAME WARNING, WHEN IT'S THE STATE VARIABLE
     *         NAME:
     *
     */

    if (strcmp (stateVar, structor->ident) == 0)
      printMsg (WARN_MSG,
                "Datatype: %s - Structor name \"%s\" is the state variable identifier",
                name,
                structor->ident);

    /*
     * [H-O] THREE CHANGES:
     *
     *       (1) EXTENDED THE "domainOK" CHECK TO THE PARAMETER COMPONENT
     *           OF THE STRUCTOR TYPE SIGNATURE
     *
     *       (2) ADDED THE LAST PARAMETER TO EACH CALL (SEE BELOW)
     *
     *       (3) ADDED A CHECK TO ENSURE CONSTRUCTORS CAN'T USE THE H-O
     *           DESTRUCTOR SYNTAX
     *
     */

    domainOK (structor->type_sig->domain,
              paramList,
              stateVar,
              "domain",
              structor->ident,
              name,
              BTRUE);

    if (structor->type_sig->param)
      if (class == DT_INDUCTIVE)
        printMsg (DELAYEDERROR_MSG,
                  "Constructor %s: Invalid constructor syntax",
                  structor->ident);
      else
        domainOK (structor->type_sig->param,
                  paramList,
                  stateVar,
                  "parameter",
                  structor->ident,
                  name,
                  BFALSE);

    domainOK (structor->type_sig->codomain,
              paramList,
              stateVar,
              "codomain",
              structor->ident,
              name,
              BTRUE);

    if (!nameClash)  /* don't repeat this check after 1 failure */
      nameClash = st_IsNameClash(structor->ident);

  } while ( (structors = StructorListTail(structors)) );
  MemDealloc(scratchHD);

  if (nameClash)
    printMsg(DELAYEDERROR_MSG, "Datatype %s: Name clashes", name);

  if (delayedErrorCount)
    return BFALSE;
  else
    return BTRUE;

}


/*********************************
 *                               *
 *    st_IsNameClash             *
 *                               *
 *********************************/
static BBOOL
st_IsNameClash(char *name) {
  /* caller can be 0 (AddFunction) or 1 (AddDatatype) */
  BBOOL      nameClash = BFALSE;
  ST_KEY     key = st_NameToKey(name);
  ST_ENTRY  *entry;

  if (key) {
    entry = st_GetEntry(key, NULL);
    switch (entry->tag) {
    case ST_FUNCTION :
      if (!gb_ReplaceFunctions) {
        printMsg(PROMPT_MSG,
                 "Name Clash - Replace function \"%s\" definition? [y] ",
                 entry->name);
        nameClash = st_HandleNameClash(entry, "Function");
      }
      else
        nameClash = BFALSE;
      if (!nameClash)
        printMsg(WARN_MSG,
                   "Function \"%s\" has been removed. Reload dependent functions.", entry->name);
      break;
    case ST_STRUCTOR :
/*      printMsg(MSG,
               "Name Clash - Structor %s already defined under datatype %s.\n",
               name, entry->info.structor.parent->name);
      printMsg(PROMPT_MSG,
               "Name Clash - Replace datatype \"%s\" definition? [y] ",
               entry->info.structor.parent->name);

      nameClash = st_HandleNameClash(entry, "Datatype");
*/
      nameClash = BTRUE;
      break;
    case ST_DATATYPE :
/*      printMsg(PROMPT_MSG,
               "Name Clash - Replace datatype \"%s\" definition? [y] ",
               entry->name);
      nameClash = st_HandleNameClash(entry, "Datatype");
*/
      nameClash = BTRUE;
      break;

    case ST_ALIAS:
    case ST_VAR :
      nameClash = BTRUE;
      break;

    case ST_MACRO    :
    case ST_OPCOMB   :
      printMsg(ERROR_MSG, "Invalid name - %s", name);
      break;
    default :
      printMsg(FATAL_MSG, "st_IsNameClash() - invalid tag %d", entry->tag);
    }
  }   /*  fi  */

  return(nameClash);

}


/*********************************
 *                               *
 *    st_HandleNameClash         *
 *                               *
 *********************************/
static BBOOL
st_HandleNameClash(ST_ENTRY *entry, char *kind) {

  char input[MAX_INPUT_LENGTH];

  emptyInputLine();  getInputLine(input,MAX_INPUT_LENGTH);  restoreInputLine();

  switch (input[0]) {
  case 'n' :
  case 'N' :
    return(BTRUE);
    break;
  default :
    st_RemoveEntry(entry->key);
    return(BFALSE);
  }

}

/*********************************
 *                               *
 *    domainOK                   *
 *                               *
 *********************************/

/*
 * [H-O] ADDED THE LAST PARAMETER, WHICH DETERMINES IF THE STATE VARIABLE IS
 *       ALLOWED TO OCCUR IN THIS "DOMAIN" (THE STATE VARIABLE IS NOT ALLOWED
 *       TO OCCUR IN THE PARAMETER PART OF A STRUCTOR TYPE SIGNATURE)
 *
 */

static
void
domainOK (PE_TYPE  *domain,           /* (co)domain of structor being tested */
          STR_LIST *paramList,        /* datatype parameters                 */
          char     *stateVar,         /* datatype state variable             */
          char     *main,             /* "domain", "codomain", or "param"    */
          char     *structName,       /* name of structor being tested       */
          char     *name,             /* name of datatype being tested       */
          BBOOL     allowStateVar)    /* is the state var allowed?           */
{
  PE_LIST_TYPE *parms = domain->parms;
  PE_TYPE      *parm;

  if (strcmp(domain->ident, stateVar)==0) {
    if (parms)
      printMsg(DELAYEDERROR_MSG,
               "Datatype: %s - State variable has parameters "
               "(%s in %s of structor %s)",
               name, stateVar, main, structName);

    /* [H-O] ADDED THE CHECK FOR STATE VARIABLE ALLOWANCE: */

    if (!allowStateVar)
      printMsg (DELAYEDERROR_MSG,
                "Datatype: %s - State variable not allowed in parameter "
                "(%s in %s of structor %s)",
                name,
                stateVar,
                main,
                structName);
  }
  else if (isDatatype(domain->ident)) {
    if (TypeListLen(parms) != getNumParams(domain->ident))
      printMsg(DELAYEDERROR_MSG,
               "Datatype: %s - Incorrect number of parameters"
               "(%s in the %s of structor %s).",
               name, domain->ident, main, structName);
    while (parms) {
      parm = TypeListHead(parms);
      parms = TypeListTail(parms);

      /* [H-O] ADDED THE LAST PARAMETER TO THE CALL (SEE ABOVE): */

      domainOK(parm,paramList,stateVar, main, structName,name,allowStateVar);
    }   /*  elihw  */
  }   /*  esle fi  */

  else if (isAlias (domain->ident))
    {
      if (TypeListLen (parms) != getNumParams (domain->ident))
        printMsg (DELAYEDERROR_MSG,
                  "Type Alias %s: invalid number of parameters",
                  domain->ident);

      while (parms)
        {
          parm  = TypeListHead (parms);
          parms = TypeListTail (parms);

          domainOK (parm,
                    paramList,
                    stateVar,
                    main,
                    structName,
                    name,
                    allowStateVar);
        }
    }

  else if (StrListMember(paramList, domain->ident)) {
    if (domain->parms)
      printMsg(DELAYEDERROR_MSG,
               "Datatype: %s - Parameter has parameters. "
               "(%s in %s of structor %s).",
               name,domain->ident, main, structName);
  }   /*  esle fi  */

  else
    printMsg(DELAYEDERROR_MSG,
             "Datatype: %s - Unknown identifier (%s in %s of structor %s).",
             name, domain->ident, main, structName);
}


/*********************************
 *                               *
 *    putTypeSig                 *
 *                               *
 *********************************/
/* assumed that everything is semantically correct at this point */
void
putTypeSig(ST_TYPE *main, PE_TYPE *peMain, STR_LIST *paramList)
{

  char *ident;
  int i,
  numParams;
  PE_LIST_TYPE  *peParms;
  ST_ENTRY      *entry;

  ident = peMain->ident;

  assert (ident);

  entry = st_GetEntry (st_NameToKey (ident), NULL);

  if (strcmp(ident, TERMINAL_TYPE)==0) {
    main->tag = TYPE_1;
  }

 else if (strcmp(ident, PROD_TYPE)==0) {
   main->tag = TYPE_PROD;
   main->info.prod.key = PROD_KEY;
   main->info.prod.l = (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
   putTypeSig(main->info.prod.l, TypeListHead(peMain->parms), paramList);
   main->info.prod.r = (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
   putTypeSig(main->info.prod.r,
              TypeListHead( TypeListTail( peMain->parms) ),
              paramList);
 }

 /* [BI] HANDLE BUILTINS: */

 else if (entry && entry->key == CHAR_KEY)
   main->tag = TYPE_BUILTIN_CHAR;

 else if (entry && entry->key == INT_KEY)
   main->tag = TYPE_BUILTIN_INT;

 else if (StrListMember(paramList, ident)) {
   main->tag = TYPE_PARAMETRIC_VAR;
   main->info.parametric_var = StrListPosn(ident, paramList);
 }

 else if (isDatatype(ident)) {
/*   entry = st_GetEntry(st_NameToKey(ident), NULL); */
   main->tag = TYPE_USER_DATA;
   main->info.user_data.name =
     MemHeapAlloc(symTab->scopeHD, strlen(ident)+1, sizeof(char));
   strcpy(main->info.user_data.name, ident);
   main->info.user_data.key = entry->key;

   numParams = getNumParams(ident);
   if (numParams) {
     main->info.user_data.args =
       (ST_TYPE **)MemHeapAlloc(symTab->scopeHD, numParams+1, sizeof(ST_TYPE *));
     peParms = peMain->parms;
     for (i=0; i<numParams; i++) {
       main->info.user_data.args[i] =
         (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
       putTypeSig(main->info.user_data.args[i],
                  TypeListHead(peParms),
                  paramList);
       peParms = TypeListTail(peParms);
     }   /*  rof  */
   }   /*  fi  */
   else   /* no parameters */
     main->info.user_data.args = NULL;
 }   /*  fi esle (isDatatype(ident))  */

  else if (isAlias (ident))
    {
      MEMORY scratch;

      ST_TYPE      *expansion = NULL;
      ST_TYPE      *newType   = NULL;
      ST_LIST_TYPE *newTypes  = NULL;

      entry = st_GetEntry (st_NameToKey (ident), NULL);

      assert (entry);
      assert (entry->info.alias.expansion);

      expansion = st_CopyType (entry->info.alias.expansion);

      assert (expansion);

      numParams = TypeListLen (peMain->parms);
      peParms   = peMain->parms;

      scratch = MemAlloc ("symtab scratch", 10000, sizeof (char *));

      for (i = 0; i < numParams; i++)
        {
          newType = (ST_TYPE *)MemHeapAlloc (symTab->scopeHD,
                                             1,
                                             sizeof (ST_TYPE));

          assert (newType);
          assert (peParms);

          putTypeSig (newType, TypeListHead (peParms), paramList);

          newTypes = StTypeListCons (newType, newTypes, scratch);

          peParms = TypeListTail (peParms);
        }

      ExpansionSubstitution2 (newTypes, &expansion, numParams);

      MemDealloc (scratch);

      /* [FIX] MEMORY LEAK */

      memcpy (main, expansion, sizeof (ST_TYPE));
    }

  else {                  /* must be a state variable */
    main->tag = TYPE_STATE_VAR;
  }

}   /*  end putTypeSig  */


/*********************************
 *                               *
 *    addTypeSigToEntry          *
 *                               *
 *********************************/

/* [H-O] ALTERED THE TYPE OF THE SECOND PARAMETER (SEE parse.h): */

static
void
addTypeSigToEntry (ST_TYPE_SIG          *typeSig,
                   PE_STRUCTOR_TYPE_SIG *peTypeSig,
                   STR_LIST             *paramList)
{
  typeSig->params   = NULL;
  typeSig->domain   = (ST_TYPE *)MemHeapAlloc (symTab->scopeHD, 1, sizeof (ST_TYPE));
  typeSig->codomain = (ST_TYPE *)MemHeapAlloc (symTab->scopeHD, 1, sizeof (ST_TYPE));

  /*
   * [H-O] ALTERED THE CREATION OF THE TYPE SIGNATURE DOMAIN WHEN IT IS OF THE
   *       EXTENDED H-O FORM, EG:
   *
   *            C -> A => B
   *
   *       INTERNALLY IS:
   *
   *            A * C -> B
   *
   *       BUT:
   *
   *            C -> B
   *
   *       INTERNALLY IS (STILL):
   *
   *            C -> B
   *
   */

  if (peTypeSig->param)
    {
      typeSig->domain->tag           = TYPE_PROD;
      typeSig->domain->info.prod.key = PROD_KEY;

      typeSig->domain->info.prod.r =
        (ST_TYPE *)MemHeapAlloc (symTab->scopeHD, 1, sizeof (ST_TYPE));
      putTypeSig (typeSig->domain->info.prod.r, peTypeSig->domain, paramList);

      typeSig->domain->info.prod.l =
        (ST_TYPE *)MemHeapAlloc (symTab->scopeHD, 1, sizeof (ST_TYPE));
      putTypeSig (typeSig->domain->info.prod.l, peTypeSig->param, paramList);
    }
  else
    putTypeSig (typeSig->domain, peTypeSig->domain, paramList);

  putTypeSig (typeSig->codomain, peTypeSig->codomain, paramList);
}


/*********************************
 *                               *
 *    putStructorEntry           *
 *                               *
 *********************************/
static
void
putStructorEntry(PE_STRUCTOR *structor,
                 STR_LIST *paramList,
                 int i,
                 ST_ENTRY *parent,
                 ST_KEY    key) {

  ST_ENTRY *structorEntry;

  structorEntry =
    (ST_ENTRY *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_ENTRY));
  structorEntry->tag = ST_STRUCTOR;
  structorEntry->name = MemHeapAlloc(symTab->scopeHD, strlen(structor->ident)+1,
                                     sizeof(char));
  strcpy(structorEntry->name, structor->ident);
  structorEntry->key = key;

  structorEntry->info.structor.posn = i;
  structorEntry->info.structor.parent = parent;

  structorEntry->info.structor.type_sig =
    (ST_TYPE_SIG *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));
  addTypeSigToEntry(structorEntry->info.structor.type_sig,
                    structor->type_sig,
                    paramList);

  /* [H-O] ADDED (SEE symtabI.h): */

  if (structor->type_sig->param)
    structorEntry->info.structor.isHO = BTRUE;
  else
    structorEntry->info.structor.isHO = BFALSE;

  addEntryToSymTab(structorEntry);

}


/*********************************
 *                               *
 *    checkDatatype              *
 *                               *
 *********************************/
static ST_DT_KIND
checkDatatype(PE_DATA *dataDefn) {

  BBOOL    inductive,
           coinductive;

  /* Is the data defn inductive or coinductive or undefined? */
  /* Ambiguous defns are not allowed. They are useless. */
  coinductive = existsStateVar(dataDefn->domainId,   dataDefn->domainVars,
                               dataDefn->codomainId, dataDefn->codomainVars,
                               dataDefn->structors,  DT_COINDUCTIVE);
  inductive = existsStateVar(dataDefn->codomainId,  dataDefn->codomainVars,
                               dataDefn->domainId,  dataDefn->domainVars,
                               dataDefn->structors, DT_INDUCTIVE);
  if (inductive && coinductive)
    printMsg(ERROR_MSG,
           "Datatype: %s or %s - Ambiguous (may be inductive or coinductive).",
             dataDefn->domainId, dataDefn->codomainId);
  else if (inductive)
    return DT_INDUCTIVE;
  else if (coinductive)
    return DT_COINDUCTIVE;
  else
    printMsg(ERROR_MSG,
             "Datatype: %s or %s - No state variable.",
             dataDefn->domainId, dataDefn->codomainId);

}   /*  end checkDatatype  */


/*********************************
 *                               *
 *    buildTypeEntry             *
 *                               *
 *********************************/
static ST_ENTRY *
buildTypeEntry (PE_DATA *dataDefn,
                ST_DT_KIND class,
                char *name,
                STR_LIST *paramList) {

  int                 numStructors   = StructorListLen(dataDefn->structors),
                      i = 0;
  PE_LIST_STRUCTOR   *structorList   = dataDefn->structors;
  PE_STRUCTOR        *structor;
  ST_ENTRY           *typeEntry;
  ST_KEY              key;

  typeEntry = (ST_ENTRY *)MHA(symTab->scopeHD,1, sizeof(ST_ENTRY));

  typeEntry->tag = ST_DATATYPE;
  typeEntry->name = libStrdup(symTab->scopeHD, name);
  typeEntry->key = st_MakeKey();

  typeEntry->info.datatype.class = class;
  typeEntry->info.datatype.numParams = StrListLen(paramList);
  typeEntry->info.datatype.numStructors = numStructors;
  typeEntry->info.datatype.structorNames =
    (char **)MemHeapAlloc(symTab->scopeHD, numStructors+1, sizeof(char *));
  typeEntry->info.datatype.structorKeys =
    (ST_KEY *)MemHeapAlloc(symTab->scopeHD, numStructors+1, sizeof(ST_KEY));

  while (structorList) {
    structor = StructorListHead(structorList);
    typeEntry->info.datatype.structorNames[i] = libStrdup(symTab->scopeHD,
                                                          structor->ident);
    key = st_MakeKey();
    typeEntry->info.datatype.structorKeys[i] = key;

    /* add structor to the symbol table */
    putStructorEntry(structor, paramList, i, typeEntry, key);

    i++;
    structorList = StructorListTail(structorList);
  }   /*  elihw  */

  typeEntry->info.datatype.genericStateType=st_MakeGenericStateType(typeEntry);

  return typeEntry;

}   /*  end buildTypeEntry */

/*
 * [H-O] ADDED THIS FUNCTION FOR PERFORMING VARIANCE CHECKING ON NEW
 *       DATATYPE DEFINITIONS:
 *
 */

static
BBOOL
VarCheck (ST_ENTRY *typeEntry)
{
  MEMORY scratch;

  int m = typeEntry->info.datatype.numParams;
  int n = typeEntry->info.datatype.numStructors;

  int i;

  V_VARIANCE *varity;
  V_VARIANCE  stateVariance;

  ST_TYPE_SIG *typeSig;

  BBOOL result;

  if (m > 0)
    {
      scratch = MemAlloc ("varCheckHD", m, sizeof (V_VARIANCE));
      varity  = (V_VARIANCE *)MemHeapAlloc (scratch, m, sizeof (V_VARIANCE));

      assert (varity);
    }
  else
    varity = NULL;

  for (i = 0; i < m; i++)
    varity[i] = V_NEITHER;

  stateVariance = V_NEITHER;

  if (typeEntry->info.datatype.class == DT_INDUCTIVE)
    for (i = 0; i < n; i++)
      {
        typeSig = st_GetTypeSig (typeEntry->info.datatype.structorKeys[i]);

        CheckType (V_POSITIVE, typeSig->domain, varity, &stateVariance);
      }
  else
    for (i = 0; i < n; i++)
      {
        typeSig = st_GetTypeSig (typeEntry->info.datatype.structorKeys[i]);

        if (typeSig->domain->tag == TYPE_PROD)
          {
            CheckType (V_NEGATIVE,
                       typeSig->domain->info.prod.l,
                       varity,
                       &stateVariance);
            CheckType (V_POSITIVE,
                       typeSig->codomain,
                       varity,
                       &stateVariance);
          }
        else
          CheckType (V_POSITIVE, typeSig->codomain, varity, &stateVariance);
      }

  if ((stateVariance != V_NEITHER) && (stateVariance != V_POSITIVE))
    {
      result = BFALSE;

      printMsg (DELAYEDERROR_MSG,
                "Datatype: %s - Illegal variance",
                typeEntry->name);
    }
  else
    result = BTRUE;

  if (m > 0)
    {
      typeEntry->info.datatype.varity
        = (V_VARIANCE *)MemHeapAlloc (symTab->scopeHD, m, sizeof (V_VARIANCE));
      memcpy (typeEntry->info.datatype.varity,
              varity,
              sizeof (V_VARIANCE) * m);

      MemDealloc (scratch);
    }
  else
    typeEntry->info.datatype.varity = NULL;

  return result;
}

/* [H-O] ADDED THIS FUNCTION ALSO: */

static
void
CheckType (V_VARIANCE  v,
           ST_TYPE    *type,
           V_VARIANCE *varity,
           V_VARIANCE *stateVariance)     /* MAY BE NULL */
{
  switch (type->tag)
    {
    case TYPE_PROD:
      CheckType (v, type->info.prod.l, varity, stateVariance);
      CheckType (v, type->info.prod.r, varity, stateVariance);

      break;

    case TYPE_PARAMETRIC_VAR:
      varity[type->info.param_var] = Meet (v, varity[type->info.param_var]);

      break;

    case TYPE_STATE_VAR:
      if (stateVariance)
        *stateVariance = Meet (v, *stateVariance);
      else
        assert (BFALSE);

      break;

    case TYPE_USER_DATA:
      {
        int         m       = st_GetNumParams (type->info.user_data.key);
        V_VARIANCE *varity2 = st_GetVarity    (type->info.user_data.key);

        int k;

        for (k = 0; k < m; k++)
          CheckType (Subst (v, varity2[k]),
                     type->info.user_data.args[k],
                     varity,
                     stateVariance);
      }

      break;
    }
}

/*********************************
 *                               *
 *    stAddVar                   *
 *                               *
 *********************************/
ST_KEY
stAddVar(char *var, BBOOL isHO) {

    ST_ENTRY   *varEntry = (ST_ENTRY *)MHA(symTab->scopeHD,1,sizeof(ST_ENTRY));

    varEntry->tag = ST_VAR;
    varEntry->name = libStrdup(symTab->scopeHD, var);
    varEntry->key = st_MakeKey();
    varEntry->info.var.uniqueVar = makeNewRsrvdVar(symTab->scopeHD);
    varEntry->info.var.isHO = isHO;

    return addEntryToSymTab(varEntry);

}   /*  stAddVar  */


/********************
 *                  *
 *    stAddMacro    *
 *                  *
 ********************/
ST_KEY
stAddMacro(char *name) {

  ST_ENTRY *entry = (ST_ENTRY *) MHA(symTab->scopeHD, 1, sizeof(ST_ENTRY));

  entry->tag = ST_MACRO;
  entry->name = libStrdup(symTab->scopeHD, name);
  entry->key = st_MakeKey();

  return addEntryToSymTab(entry);
}


/*********************************
 *                               *
 *    addDatatype                *
 *                               *
 *********************************/
void
addDatatype(PE_DATA *dataDefn) {

  ST_DT_KIND     class;
  STR_LIST   *paramList;
  char          *stateVar,
                *name;
  ST_ENTRY      *typeEntry;
  ST_KEY         typeKey;

  class = checkDatatype(dataDefn);
  if (class == DT_INDUCTIVE) {
    stateVar = dataDefn->codomainId;
    name = dataDefn->domainId;
    paramList = dataDefn->domainVars;
  }
  else {
    stateVar = dataDefn->domainId;
    name = dataDefn->codomainId;
    paramList = dataDefn->codomainVars;
  }

  /* Are the structor type signatures valid? */

  /* [H-O] ADDED THE LAST PARAMETER (SEE ABOVE): */

  if (areValidStructDefns (dataDefn->structors,
                           name,
                           paramList,
                           stateVar,
                           st_IsNameClash (name),
                           class)) {
    typeEntry = buildTypeEntry(dataDefn, class, name, paramList);

    /* [H-O] ADDED THIS CALL TO THE VARIANCE CHECKER (SEE ABOVE): */

    /* [FIX] IF VARIANCE CHECK FAILS, SYSTEM CORE DUMPS UPON QUERY */

    if (VarCheck (typeEntry))
      {
        typeKey = addEntryToSymTab(typeEntry);

        typeEntry->info.datatype.opCombKeys =
          (ST_KEY *)MHA(symTab->scopeHD, NUM_OPCOMBS, sizeof(ST_KEY));
        st_AddOpCombs(typeKey);

        /* [H-O] [CLEAN] PRINTING SHOULD BE THROUGH THE I/O MODULE: */

        printf ("\nDatatype added: %s", typeEntry->name);

        if (typeEntry->info.datatype.numParams > 0)
          {
            int i;

            printf (" [ ");

            for (i = 0; i < typeEntry->info.datatype.numParams; i++)
              switch (typeEntry->info.datatype.varity[i])
                {
                case V_NEITHER:
                  printf ("? ");

                  break;

                case V_POSITIVE:
                  printf ("+ ");

                  break;

                case V_NEGATIVE:
                  printf ("- ");

                  break;

                case V_BOTH:
                  printf ("* ");

                  break;
                }

            printf ("]\n\n");
          }
        else
          printf (" []\n\n");
      }
    else
      delayedErrorCount = 0;
  }
  else
    delayedErrorCount = 0;

}   /*  end of addDataType  */


/*********************************
 *                               *
 *    addAlias                   *
 *                               *
 *********************************/

void
addAlias (PE_ALIAS *alias)
{
  ST_ENTRY *entry     = NULL;
  int       numParams = 0;
  int       i         = 0;

  assert (alias);

  if (st_IsNameClash (alias->name))
    {
      printMsg (ERROR_MSG, "Type alias %s: name clashes", alias->name);
      return;
    }

  CheckBindings (alias->variables, alias->type);

  entry = (ST_ENTRY *)MemHeapAlloc (symTab->scopeHD, 1, sizeof (ST_ENTRY));

  assert (entry);

  entry->name = (char *)MemHeapAlloc (symTab->scopeHD,
                                      strlen (alias->name) + 1,
                                      sizeof (char));

  assert (entry->name);

  strcpy (entry->name, alias->name);

  entry->tag       = ST_ALIAS;
  entry->key       = st_MakeKey ();
  entry->nextEntry = NULL;

  numParams = entry->info.alias.numParams = StrListLen (alias->variables);
  entry->info.alias.expansion = st_PETypeToEntryType (alias->type,
                                                      &alias->variables);

  if (numParams > 0)
    {
      entry->info.alias.varity =
        (V_VARIANCE *)MemHeapAlloc (symTab->scopeHD,
                                    numParams,
                                    sizeof (V_VARIANCE));

      assert (entry->info.alias.varity);

      for (i = 0; i < numParams; i++)
        entry->info.alias.varity[i] = V_NEITHER;

      CheckType (V_POSITIVE,
                 entry->info.alias.expansion,
                 entry->info.alias.varity,
                 NULL);
    }
  else
    entry->info.alias.varity = NULL;

  addEntryToSymTab (entry);

  printf ("\nType alias added: %s", entry->name);

  if (numParams > 0)
    {
      printf (" [ ");

      for (i = 0; i < numParams; i++)
        switch (entry->info.alias.varity[i])
          {
          case V_NEITHER:
            printf ("? ");

            break;

          case V_POSITIVE:
            printf ("+ ");

            break;

          case V_NEGATIVE:
            printf ("- ");

            break;

          case V_BOTH:
            printf ("* ");

            break;
          }

      printf ("]\n\n");
    }
  else
    printf (" []\n\n");
}


/*********************************
 *                               *
 *    CheckBindings              *
 *                               *
 *********************************/

static
void
CheckBindings (STR_LIST *variables,
               PE_TYPE  *type)
{
  ST_KEY    key   = st_NameToKey (type->ident);
  ST_ENTRY *entry = NULL;
  BBOOL     check = BFALSE;

  assert (type);

  if (key)
    {
      entry = st_GetEntry (key, NULL);

      switch (entry->tag)
        {
        case ST_DATATYPE:
        case ST_ALIAS:
          if (type->parms)
            {
              PE_LIST_TYPE *types = type->parms;

              do
                {
                  CheckBindings (variables, TypeListHead (types));

                  types = TypeListTail (types);
                }
              while (types);
            }

          break;

        case ST_FUNCTION:
        case ST_MACRO:
        case ST_STRUCTOR:
        case ST_OPCOMB:
        case ST_VAR:
          check = BTRUE;

          break;

        default:
          assert (BFALSE);
        }
    }
  else
    check = BTRUE;

  if (check && !StrListMember (variables, type->ident))
    printMsg (ERROR_MSG, "Type alias contains unbound variables");
}


/*********************************
 *                               *
 *    ExpandAlias                *
 *                               *
 *********************************/

static
ST_TYPE *
ExpandAlias (PE_TYPE   *alias,
             ST_ENTRY  *aliasEntry,
             STR_LIST **pNameList)
{
  ST_TYPE *expansion = NULL;
  int      numParams = 0;

  assert (alias);
  assert (aliasEntry);
  assert (aliasEntry->tag == ST_ALIAS);

  /* EXPANSION IS NULL <=> TYPE ALIAS IS RECURSIVE */

  if (!aliasEntry->info.alias.expansion)
    {
      st_RemoveEntry (aliasEntry->key);

      printMsg (ERROR_MSG, "Type alias %s is recursive", aliasEntry->name);
    }

  expansion = st_CopyType (aliasEntry->info.alias.expansion);

  assert (expansion);

  numParams = TypeListLen (alias->parms);

  if (numParams > 0)
    ExpansionSubstitution (alias, &expansion, numParams, pNameList);

  return expansion;
}


/*********************************
 *                               *
 *    ExpansionSubstitution      *
 *                               *
 *********************************/

static
void
ExpansionSubstitution (PE_TYPE   *alias,
                       ST_TYPE  **expansion,
                       int        numParams,
                       STR_LIST **pNameList)
{
  switch ((*expansion)->tag)
    {
    case TYPE_PARAMETRIC_VAR:
      {
/*
        int position =
          (numParams - (int)(*expansion)->info.param_var) - 1;
*/

        int position = (int)(*expansion)->info.param_var;

        /* [FIX] MEMORY LEAK */

        *expansion =
          st_PETypeToEntryType (TypeListIndex (alias->parms, position),
                                pNameList);
      }

      break;

    case TYPE_PROD:
      ExpansionSubstitution (alias,
                             &(*expansion)->info.prod.l,
                             numParams,
                             pNameList);
      ExpansionSubstitution (alias,
                             &(*expansion)->info.prod.r,
                             numParams,
                             pNameList);

      break;

    case TYPE_USER_DATA:
      {
        int numDataParams = st_GetNumParams ((*expansion)->info.user_data.key);
        int index         = 0;

        for (index = 0; index < numDataParams; index++)
          ExpansionSubstitution (alias,
                                 &(*expansion)->info.user_data.args[index],
                                 numParams,
                                 pNameList);
      }

      break;

    case TYPE_1:
      break;

    default:
      assert (BFALSE);
    }
}


/*********************************
 *                               *
 *    ExpansionSubstitution2     *
 *                               *
 *********************************/

static
void
ExpansionSubstitution2 (ST_LIST_TYPE  *types,
                        ST_TYPE      **expansion,
                        int            numParams)
{
  switch ((*expansion)->tag)
    {
    case TYPE_PARAMETRIC_VAR:
      /* [FIX] MEMORY LEAK */

      memcpy (*expansion,
              StTypeListIndex (types,
                               (numParams - (int)(*expansion)->info.param_var) - 1),
              sizeof (ST_TYPE));

      break;

    case TYPE_PROD:
      ExpansionSubstitution2 (types, &(*expansion)->info.prod.l, numParams);
      ExpansionSubstitution2 (types, &(*expansion)->info.prod.r, numParams);

      break;

    case TYPE_USER_DATA:
      {
        int numParams = st_GetNumParams ((*expansion)->info.user_data.key);
        int index     = 0;

        for (index = 0; index < numParams; index++)
          ExpansionSubstitution2 (types,
                                  &(*expansion)->info.user_data.args[index],
                                  numParams);
      }

      break;

    case TYPE_1:
    case TYPE_BUILTIN_INT:
    case TYPE_BUILTIN_CHAR:
      break;

    default:
      assert (BFALSE);
    }
}


static
ST_LIST_TYPE *
StTypeListCons (ST_TYPE      *x,
                ST_LIST_TYPE *xs,
                MEMORY        hd)
{
  assert (x);

  return (ST_LIST_TYPE *)_cons ((char *)x, (LIST *)xs, L_ST_TYPE, hd);
}

static
ST_TYPE *
StTypeListHead (ST_LIST_TYPE *xs)
{
  assert (xs);

  return (ST_TYPE *)ListHead ((LIST *)xs);
}

static
ST_LIST_TYPE *
StTypeListTail (ST_LIST_TYPE *xs)
{
  assert (xs);

  return (ST_LIST_TYPE *)ListTail ((LIST *)xs);
}

static
ST_TYPE *
StTypeListIndex (ST_LIST_TYPE *xs,
                 int           index)
{
  assert (xs);
  assert (index >= 0);

  while (index > 0)
    {
      index--;
      xs = StTypeListTail (xs);

      assert (xs);
    }

  return StTypeListHead (xs);
}


/*********************************
 *                               *
 *    st_AddFunction             *
 *                               *
 *********************************/
ST_KEY
st_AddFunction(PE_DEF *def) {

  if (!st_IsNameClash(def->id))
    return(_st_AddFunction(def));
  else
    printMsg(ERROR_MSG, "Function %s: Name clashes", def->id);

}


/*********************************
 *                               *
 *    _st_AddFunction            *
 *                               *
 *********************************/
ST_KEY
_st_AddFunction(PE_DEF *def) {

  ST_ENTRY       *funEntry = (ST_ENTRY *)MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_ENTRY));
  ST_TYPE_SIG    *fSig = st_DefSigToEntrySig(def->type_sig, def->macros);
  PE_LIST_MACRO  *macros = def->macros;
  PE_MACRO       *macro;
  char           *fName  = def->id;
  int             numMacros = MacroListLen(def->macros),
                  i;

  funEntry->tag = ST_FUNCTION;
  funEntry->name = (char *)MemHeapAlloc(symTab->scopeHD, strlen(fName)+1,sizeof(char));
  strcpy(funEntry->name, fName);
  funEntry->key  = st_MakeKey();
  funEntry->nextEntry = NULL;

  addEntryToSymTab(funEntry);

  funEntry->info.function.instr = MCinvalid;     /* [BI] ADDED (SEE miscSymtab.c) */

  funEntry->info.function.type_sig  = fSig;
  funEntry->info.function.numMacros = numMacros;
  funEntry->info.function.macroNames =
    (char **)MHA(symTab->scopeHD, numMacros+1, sizeof(char *));
  funEntry->info.function.macroNames[numMacros] = NULL;
  funEntry->info.function.macroKeys =
    (ST_KEY *)MHA(symTab->scopeHD, numMacros+1, sizeof(ST_KEY));
  funEntry->info.function.macroKeys[numMacros] = 0;

  for (i=0; i<numMacros; i++) {
    macro = MacroListHead(macros);
    macros = MacroListTail(macros);
    funEntry->info.function.macroNames[i] =
      lb_BuildMacroName(symTab->scopeHD, fName, macro->ident);
    funEntry->info.function.macroKeys[i] =
      st_AddMacro(funEntry->key, macro, fSig->params[i], i);
  }

  return(funEntry->key);

}


/*********************************
 *                               *
 *    st_AddMacro                *
 *                               *
 *********************************/
static ST_KEY
st_AddMacro(ST_KEY funKey,
            PE_MACRO *macro,
            ST_TYPE_SIG *mSig,
            int i) {

  ST_ENTRY  *macEntry = (ST_ENTRY *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_ENTRY));
  ST_ENTRY  *funEntry = st_GetEntry(funKey, NULL);

  macEntry->tag = ST_MACRO;
  macEntry->name = lb_BuildMacroName(symTab->scopeHD, funEntry->name, macro->ident);
  macEntry->key  = st_MakeKey();
  macEntry->nextEntry = NULL;

  macEntry->info.macros.posn = i;
  macEntry->info.macros.parent_type = funEntry;
  macEntry->info.macros.type_sig = mSig;

  addEntryToSymTab(macEntry);

  return(macEntry->key);

}


/*********************************
 *                               *
 *    st_DefSigToEntrySig        *
 *                               *
 *********************************/
static ST_TYPE_SIG *
st_DefSigToEntrySig(PE_TYPE_SIG *defSig, PE_LIST_MACRO *macros) {

  ST_TYPE_SIG   *sig = (ST_TYPE_SIG *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));
  PE_MACRO      *macro;
  ST_PVAR        lastParam = 0;
  ST_PVAR        contextParam;
  int            numMacros = MacroListLen(macros),
                 i;
  STR_LIST     *pNameList = NULL;

  scratchHD = MemAlloc("symtab scratch2", 100, sizeof(char *));

  if (!defSig) {
    sig = st_NullSigToEntrySig(&lastParam);
    sig->userspecified = BFALSE;
  }
  else {
    sig = st_PESigToEntrySig(defSig, &pNameList);
    sig->userspecified = BTRUE;
  }
  sig->params = (ST_TYPE_SIG **)MHA(symTab->scopeHD, numMacros+1, sizeof(ST_TYPE_SIG *));
  sig->params[numMacros] = NULL;

  for(i=0; i<numMacros; i++) {
    macro  = MacroListHead(macros);
    macros = MacroListTail(macros);
    if (!macro->type_sig) {
      sig->params[i] = st_NullSigToEntrySig(&lastParam);
      sig->params[i]->userspecified = BFALSE;
    }
    else {
      sig->params[i] = st_PESigToEntrySig(macro->type_sig, &pNameList);
      sig->params[i]->userspecified = BTRUE;
    }
/*    printMsg(DEBUG_MSG, "Macro %s type sig : %t",macro->ident, sig->params[i]); */
  }

  /*  rename all non-userspecified type vars */
  /*   to make sure they don't clash with    */
  /*    userspecified type vars              */

  contextParam = StrListLen(pNameList);
  lastParam = contextParam+1;
  if (sig->userspecified == BFALSE) {
    sig->domain->info.param_var = lastParam++;
    sig->codomain->info.param_var = lastParam++;
  }

#if 0
not needed for term logic typechecking
  /* add context to function domain (only if it has > 0 macros) */

  if (numMacros > 0) {
    st_type = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
    st_type->tag = TYPE_PROD;
    st_type->info.prod.key = PROD_KEY;
    st_type->info.prod.l = sig->domain;
    st_type->info.prod.r = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
    st_type->info.prod.r->tag = TYPE_PARAMETRIC_VAR;
    st_type->info.prod.r->info.param_var = contextParam;
    sig->domain = st_type;
  }
#endif

  for (i=0; i<numMacros; i++) {
    if (sig->params[i]->userspecified == BFALSE) {
        sig->params[i]->domain->info.param_var = lastParam++;
        sig->params[i]->codomain->info.param_var = lastParam++;
    }
}
#if 0
not needed for term logic typechecking
    /* add context to macro domains: */

    st_type = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
    st_type->tag = TYPE_PROD;
    st_type->info.prod.key = PROD_KEY;
    st_type->info.prod.l = sig->params[i]->domain;
    st_type->info.prod.r = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
    st_type->info.prod.r->tag = TYPE_PARAMETRIC_VAR;
    st_type->info.prod.r->info.param_var = contextParam;
    sig->params[i]->domain = st_type;
  }
#endif

  st_TranslateCleanup();
/*  printMsg(DEBUG_MSG, "Function type sig : %t", sig); */
  return(sig);

}


/*********************************
 *                               *
 *    st_PESigToEntrySig         *
 *                               *
 *********************************/
static ST_TYPE_SIG *
st_PESigToEntrySig(PE_TYPE_SIG *PESig, STR_LIST **pNameList) {

  ST_TYPE_SIG *sig = (ST_TYPE_SIG *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));

  assert(PESig);

  sig->params = NULL;
  sig->domain   = st_PETypeToEntryType(PESig->domain, pNameList);
  sig->codomain = st_PETypeToEntryType(PESig->codomain, pNameList);

  return(sig);

}


/*********************************
 *                               *
 *    st_PETypeToEntryType       *
 *                               *
 *********************************/
static ST_TYPE *
st_PETypeToEntryType(PE_TYPE *PEMain, STR_LIST **pNameList) {

  ST_TYPE *main;
  ST_KEY   key  = st_NameToKey(PEMain->ident);
  ST_ENTRY *entry;

  if (key) {
    entry = st_GetEntry(key, NULL);
    switch (entry->tag) {
    case ST_DATATYPE :
      main = st_HandleDatatype(PEMain->parms, entry, pNameList);
      break;

    case ST_ALIAS:
      if (TypeListLen (PEMain->parms) != entry->info.alias.numParams)
        printMsg (ERROR_MSG, "Invalid number of parameters");

      main = ExpandAlias (PEMain, entry, pNameList);

      break;

    case ST_FUNCTION :
    case ST_MACRO :
    case ST_STRUCTOR :
    case ST_OPCOMB :
      main = st_HandleParamType(PEMain->ident, pNameList);
      break;
    case ST_VAR:
      break;
    default :
     printMsg(FATAL_MSG,"st_PETypeToEntryType() - invalid tag %d", entry->tag);
    }
  }
  else
    main = st_HandleParamType(PEMain->ident, pNameList);

  return(main);

}


/*********************************
 *                               *
 *    st_HandleDatatype          *
 *                               *
 *********************************/
static
ST_TYPE *
st_HandleDatatype (PE_LIST_TYPE *parms, ST_ENTRY *entry, STR_LIST **pNameList)
{
  ST_TYPE *type = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
  int      len  = TypeListLen(parms);
  int      numParams = entry->info.datatype.numParams;
  int      i;

  if (entry->key == PROD_KEY) {
    type->tag = TYPE_PROD;
    if (len == 2) {
      type->info.prod.key = PROD_KEY;
      type->info.prod.l = st_PETypeToEntryType(TypeListHead(parms), pNameList);
      parms = TypeListTail(parms);
      type->info.prod.r = st_PETypeToEntryType(TypeListHead(parms), pNameList);
    }
    else {
      st_TranslateCleanup();
      printMsg(ERROR_MSG, "In use defined type, " PROD_TYPE
               " has %d arguments (should be 2).", len);
    }
  }
  else if (entry->key == TERM_KEY) {
    type->tag = TYPE_1;
    if (len != 0) {
      st_TranslateCleanup();
      printMsg(ERROR_MSG, "In user defined type, " TERMINAL_TYPE
               " has %d arguments (should be 0).", len);
    }
  }

  /* [BI] HANDLE BUILTINS: */

  else if (entry->key == CHAR_KEY)
    {
      type->tag = TYPE_BUILTIN_CHAR;

      if (len != 0)
        {
          st_TranslateCleanup ();
          printMsg (ERROR_MSG, "Type " CHAR_TYPENAME " has 0 parameters");
        }
    }
  else if (entry->key == INT_KEY)
    {
      type->tag = TYPE_BUILTIN_INT;

      if (len != 0)
        {
          st_TranslateCleanup ();
          printMsg (ERROR_MSG, "Type " INT_TYPENAME " has 0 parameters");
        }
    }

  else {
    type->tag = TYPE_USER_DATA;
    if (len == entry->info.datatype.numParams) {
      type->info.user_data.name = entry->name;
      type->info.user_data.key = entry->key;
      type->info.user_data.args =
        (ST_TYPE **)MHA(symTab->scopeHD, numParams+1, sizeof(ST_TYPE *));
      type->info.user_data.args[numParams] = NULL;
      for (i=0; i<numParams; i++) {
        type->info.user_data.args[i] =
          st_PETypeToEntryType(TypeListHead(parms), pNameList);
        parms = TypeListTail(parms);
      }
    }   /*  fi  */
    else {
      st_TranslateCleanup();
      printMsg(ERROR_MSG, "In user defined type, %s has %d arguments (should "
               "be %d).", entry->name, len, entry->info.datatype.numParams);
    }   /*  esle  */
  }   /*  esle  */

  return(type);

}


/*********************************
 *                               *
 *    st_HandleParamType         *
 *                               *
 *********************************/

static
ST_TYPE *
st_HandleParamType (char      *ident,
                    STR_LIST **pNameList)
{
  ST_TYPE *type = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
  int      posn = StrListPosn(ident, *pNameList),
           len  = StrListLen(*pNameList);

  type->tag = TYPE_PARAMETRIC_VAR;

  if (posn == -1)
    {
      *pNameList = StrListCons(ident, *pNameList, scratchHD);
      type->info.param_var = (ST_PVAR)len;
    }
  else
    /* need to normalize the posn to a PVAR value */
    type->info.param_var = (ST_PVAR)(len - (1 + posn));

  return type;
}


/*********************************
 *                               *
 *    st_NullSigToEntrySig       *
 *                               *
 *********************************/
static ST_TYPE_SIG *
st_NullSigToEntrySig(ST_PVAR *lastParam) {

  ST_TYPE_SIG *sig = (ST_TYPE_SIG *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));

  sig->params = NULL;
  sig->domain = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
  sig->domain->tag = TYPE_PARAMETRIC_VAR;
  sig->domain->info.param_var = *lastParam;

  sig->codomain = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
  sig->codomain->tag = TYPE_PARAMETRIC_VAR;
  sig->codomain->info.param_var = *lastParam + 1;

  *lastParam = *lastParam + 2;
  return(sig);

}


/*********************************
 *                               *
 *    st_TranslateCleanup        *
 *                               *
 *********************************/
static void
st_TranslateCleanup(void) {

  MemDealloc(scratchHD);

}


/*********************************
 *                               *
 *    st_MakeKey                 *
 *                               *
 *********************************/

ST_KEY
st_MakeKey (void)
{
  return (ST_KEY)getUniqueInt ();
}


/*********************************
 *                               *
 *    st_AddOpCombs              *
 *                               *
 *********************************/
void
st_AddOpCombs(ST_KEY typeKey) {

  ST_ENTRY *typeEntry    = st_GetEntry(typeKey, NULL),
           *combEntry;
  ST_KEY   *structKeys;
  char     *name;
  int       numStructs,
            numParams;
  ST_DT_KIND class;

  assert(typeEntry);
  name       = typeEntry->name;
  structKeys = typeEntry->info.datatype.structorKeys;
  numStructs = typeEntry->info.datatype.numStructors;
  numParams  = typeEntry->info.datatype.numParams;
  class      = typeEntry->info.datatype.class;

  if (class == DT_INDUCTIVE) {
    combEntry = st_MakeOpComb("case", name, typeEntry, 0);
    combEntry->info.opcomb.numParams = numStructs;
    combEntry->info.opcomb.type_sig =
      st_MakeCaseTypeSig(combEntry, structKeys, numStructs);

    combEntry = st_MakeOpComb("fold", name, typeEntry, 1);
    combEntry->info.opcomb.numParams = numStructs;
    combEntry->info.opcomb.type_sig =
      st_MakeFoldTypeSig(combEntry, structKeys, numStructs);
  }
  else if (class == DT_COINDUCTIVE) {
    combEntry = st_MakeOpComb("record", name, typeEntry, 0);
    combEntry->info.opcomb.numParams = numStructs;
    combEntry->info.opcomb.type_sig =
      st_MakeRecordTypeSig(combEntry, structKeys, numStructs);

    combEntry = st_MakeOpComb("unfold", name, typeEntry, 1);
    combEntry->info.opcomb.numParams = numStructs;
    combEntry->info.opcomb.type_sig =
      st_MakeUnfoldTypeSig(combEntry, structKeys, numStructs);
  }
  combEntry = st_MakeOpComb("map", name, typeEntry, 2);
  combEntry->info.opcomb.numParams = numParams;

  /* [H-O] ADDED THE THIRD PARAMETER TO THE CALL (SEE BELOW): */

  combEntry->info.opcomb.type_sig =
    st_MakeMapTypeSig (combEntry, numParams, typeEntry->info.datatype.varity);
}


/*********************************
 *                               *
 *    st_MakeOpComb              *
 *                               *
 *********************************/
static ST_ENTRY *
st_MakeOpComb(char *combName, char *name, ST_ENTRY *parent, int posn){

  ST_ENTRY     *combEntry = (ST_ENTRY *)MemHeapAlloc(symTab->scopeHD,1, sizeof(ST_ENTRY));

  combEntry->tag = ST_OPCOMB;
  combEntry->name = lb_BuildCombString(symTab->scopeHD, combName, name);
  combEntry->key = st_MakeKey();
  parent->info.datatype.opCombKeys[posn] = combEntry->key;
  combEntry->nextEntry = NULL;
  combEntry->info.opcomb.parent = parent;

  addEntryToSymTab(combEntry);

  return(combEntry);

}


/*********************************
 *                               *
 *    st_MakeCaseTypeSig         *
 *                               *
 *********************************/
static ST_TYPE_SIG *
st_MakeCaseTypeSig(ST_ENTRY *combEntry, ST_KEY *structKeys, int numStructs) {

  ST_TYPE_SIG  *sig;
  ST_PVAR       env = combEntry->info.opcomb.parent->info.datatype.numParams,
                cod = combEntry->info.opcomb.parent->info.datatype.numParams+1;

  sig = st_MakeGenCombTypeSig(structKeys, numStructs, combEntry->info.opcomb.parent->info.datatype.genericStateType, env, cod);

  return(sig);

}


/*********************************
 *                               *
 *    st_MakeFoldTypeSig         *
 *                               *
 *********************************/
static ST_TYPE_SIG *
st_MakeFoldTypeSig(ST_ENTRY *combEntry, ST_KEY *structKeys, int numStructs) {

  ST_TYPE_SIG  *sig;
  ST_PVAR       env = combEntry->info.opcomb.parent->info.datatype.numParams,
                cod = combEntry->info.opcomb.parent->info.datatype.numParams+1;
  ST_TYPE      *codomainType = (ST_TYPE *)MHA(symTab->scopeHD,1,sizeof(ST_TYPE));

  codomainType->tag = TYPE_PARAMETRIC_VAR;
  codomainType->info.param_var = cod;

  sig = st_MakeGenCombTypeSig(structKeys, numStructs, codomainType, env, cod);

  return(sig);

}


/*********************************
 *                               *
 *    st_MakeMapTypeSig          *
 *                               *
 *********************************/

/* [H-O] ALTERED THIS FUNCTION TO HANDLE MULTIVARIANT MAPS: */

static
ST_TYPE_SIG *
st_MakeMapTypeSig (ST_ENTRY   *combEntry,
                   int         numParams,
                   V_VARIANCE *varity)        /* MAY BE NULL */
{
  ST_TYPE_SIG  *sig    = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG **params = NULL;

  ST_PVAR env = combEntry->info.opcomb.parent->info.datatype.numParams * 2;

  ST_TYPE *envType           = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *first             = NULL;
  ST_TYPE *second            = NULL;
  ST_TYPE *gst               =
    combEntry->info.opcomb.parent->info.datatype.genericStateType;

  int index = 0;

  assert (sig);
  assert (envType);

  envType->tag            = TYPE_PARAMETRIC_VAR;
  envType->info.param_var = env;

  if (numParams)
    {
      params = (ST_TYPE_SIG **)MHA (symTab->scopeHD,
                                    numParams + 1,
                                    sizeof (ST_TYPE_SIG *));

      assert (params);

      params[numParams] = NULL;
    }

  for (index = 0; index < numParams; index++)
    {
      params[index] = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));

      assert (params[index]);

      if (varity[index] != V_NEITHER)
        {
          first  = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
          second = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));

          assert (first);
          assert (second);

          first->tag             = TYPE_PARAMETRIC_VAR;
          first->info.param_var  = index;

          second->tag            = TYPE_PARAMETRIC_VAR;
          second->info.param_var = index + numParams;
        }

      if (varity[index] == V_POSITIVE || varity[index] == V_BOTH)
        {
          params[index]->domain   = st_MakeProdST_Type (first, envType);
          params[index]->codomain = second;
        }
      else
        {
          params[index]->domain   = NULL;
          params[index]->codomain = NULL;
        }

      if (varity[index] == V_NEGATIVE || varity[index] == V_BOTH)
        {
          params[index]->negDomain   = st_MakeProdST_Type (second, envType);
          params[index]->negCodomain = first;
        }
      else
        {
          params[index]->negDomain   = NULL;
          params[index]->negCodomain = NULL;
        }

      params[index]->params        = NULL;
      params[index]->userspecified = BFALSE;
    }

  sig->domain   = st_MakeProdST_Type (gst, envType);
  sig->codomain = st_ChangeParamVars (gst, numParams);

  sig->negDomain   = NULL;
  sig->negCodomain = NULL;

  sig->params = params;

  sig->userspecified = BFALSE;

  return sig;
}


/*********************************
 *                               *
 *    st_MakeUnfoldTypeSig       *
 *                               *
 *********************************/

/*
 * [H-O] ALTERED THIS FUNCTION TO GENERATE TYPE SIGNATURES FOR H-O DATATYPE
 *       UNFOLDS
 *
 *       EG: data C -> R(A) = d_i : C -> F_i(A, C)
 *                          |     .
 *                          |     .
 *                          |     .
 *                          | d_j : C -> E_j(A) => F_j(A, C).
 *
 *           YIELDS:
 *
 *           unfold^R {C * E -> F_i(A, C),
 *                     ...,
 *                     E_j(A) * (C * E) -> F_j(A, C)}: C * E -> R(A)
 *
 */

static
ST_TYPE_SIG *
st_MakeUnfoldTypeSig (ST_ENTRY *combEntry,
                      ST_KEY   *structKeys,
                      int       numStructs)
{
  ST_TYPE_SIG  *sig    = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG **params = (ST_TYPE_SIG **) MHA (symTab->scopeHD,
                                               numStructs + 1,
                                               sizeof (ST_TYPE_SIG *));

  int i;

  ST_TYPE *domain;
  ST_TYPE *codomain;

  ST_TYPE *stateType = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *envType   = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));

  int     numParams = st_GetNumParams (combEntry->info.opcomb.parent->key);
  ST_PVAR state     = numParams;
  ST_PVAR env       = numParams + 1;

  ST_ENTRY *structEntry;
  ST_TYPE  *structDomain;
  ST_TYPE  *structCodomain;

  envType->tag              = TYPE_PARAMETRIC_VAR;
  envType->info.param_var   = env;
  stateType->tag            = TYPE_PARAMETRIC_VAR;
  stateType->info.param_var = state;

  domain   = st_MakeProdST_Type (stateType, envType);
  codomain = combEntry->info.opcomb.parent->info.datatype.genericStateType;

  for (i = 0; i < numStructs; i++)
    {
      params[i] = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));

      params[i]->params = NULL;

      structEntry = st_GetEntry (structKeys[i], NULL);

      structDomain      = structEntry->info.structor.type_sig->domain;
      params[i]->domain = st_ReplaceStateVar (structDomain, domain);

/*
      structDomain      = structEntry->info.structor.type_sig->domain;
      structDomain2     = st_ReplaceStateVar (structDomain, stateType);
      envType2          = st_ReplaceStateVar (envType, NULL);
      params[i]->domain = st_MakeProdST_Type (structDomain2, envType2);
*/

      structCodomain      = structEntry->info.structor.type_sig->codomain;
      params[i]->codomain = st_ReplaceStateVar (structCodomain, stateType);
    }

  params[numStructs] = NULL;

  sig->params   = params;
  sig->domain   = domain;
  sig->codomain = codomain;

  return (sig);
}


/*********************************
 *                               *
 *    st_MakeRecordTypeSig       *
 *                               *
 *********************************/

/*
 * [H-O] ALTERED THIS FUNCTION TO GENERATE TYPE SIGNATURES FOR H-O DATATYPE
 *       RECORDS
 *
 *       EG: data C -> R(A) = d_i : C -> F_i(A, C)
 *                          |     .
 *                          |     .
 *                          |     .
 *                          | d_j : C -> E_j(A) => F_j(A, C).
 *
 *           YIELDS:
 *
 *           record^R {E -> F_i(A, R(A)),
 *                     ...,
 *                     E_j(A) * E -> F_j(A, R(A))}: E -> R(A)
 *
 */

static
ST_TYPE_SIG *
st_MakeRecordTypeSig (ST_ENTRY *combEntry,
                      ST_KEY   *structKeys,
                      int       numStructs)
{
  ST_TYPE_SIG  *sig    = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG **params = (ST_TYPE_SIG **)MHA (symTab->scopeHD,
                                              numStructs + 1,
                                              sizeof (ST_TYPE_SIG *));

  int i;

  ST_TYPE *gst = combEntry->info.opcomb.parent->info.datatype.genericStateType;

  ST_TYPE *envType = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));

  int     numParams = st_GetNumParams (combEntry->info.opcomb.parent->key);
  ST_PVAR env       = numParams;

  ST_ENTRY *structEntry;
  ST_TYPE  *structDomain;
  ST_TYPE  *structCodomain;

  envType->tag            = TYPE_PARAMETRIC_VAR;
  envType->info.param_var = env;

  for (i = 0; i < numStructs; i++)
    {
      params[i] = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));

      params[i]->params = NULL;

      structEntry = st_GetEntry (structKeys[i], NULL);

      structDomain      = structEntry->info.structor.type_sig->domain;
      params[i]->domain = st_ReplaceStateVar (structDomain, envType);

      /* THIS IS A NEAT LITTLE WORK-SAVING HACK, BE CAREFUL IF MODIFYING: */
/*
      structDomain  = structEntry->info.structor.type_sig->domain;
      structDomain2 = st_ReplaceStateVar (structDomain, envType);

      if (structDomain2->tag == TYPE_PROD)
        {
          ST_TYPE *temp;

          temp                       = structDomain2->info.prod.l;
          structDomain2->info.prod.l = structDomain2->info.prod.r;
          structDomain2->info.prod.r = temp;
        }

      params[i]->domain = structDomain2;
*/
      structCodomain      = structEntry->info.structor.type_sig->codomain;
      params[i]->codomain = st_ReplaceStateVar (structCodomain, gst);
    }

  params[numStructs] = NULL;

  sig->params   = params;
  sig->domain   = envType;
  sig->codomain = gst;

  return (sig);
}


/*********************************
 *                               *
 *    st_MakeGenCombTypeSig      *
 *                               *
 *********************************/
static ST_TYPE_SIG *
st_MakeGenCombTypeSig(ST_KEY *structKeys,
                      int numStructs,
                      ST_TYPE *rplcmnt,
                      ST_PVAR env,
                      ST_PVAR cod) {

  ST_TYPE_SIG  *sig = (ST_TYPE_SIG *)MemHeapAlloc(symTab->scopeHD,1, sizeof(ST_TYPE_SIG));
  ST_TYPE_SIG **params =
    (ST_TYPE_SIG **)MemHeapAlloc(symTab->scopeHD, numStructs+1, sizeof(ST_TYPE_SIG *));
  ST_TYPE      *left,
               *right;
  ST_ENTRY     *entry;
  int           i;

  for (i=0; i<numStructs; i++) {
    params[i] = (ST_TYPE_SIG *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));
    params[i]->params = NULL;

    entry = st_GetEntry(structKeys[i], NULL);
    left = st_ReplaceStateVar(entry->info.structor.type_sig->domain, rplcmnt);
    right = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
    right->tag = TYPE_PARAMETRIC_VAR;
    right->info.param_var = env;
    params[i]->domain = st_MakeProdST_Type(left, right);

    params[i]->codomain = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
    params[i]->codomain->tag = TYPE_PARAMETRIC_VAR;
    params[i]->codomain->info.param_var = cod;
  }   /*  rof  */
  params[numStructs] = NULL;
  sig->params = params;

  left = entry->info.structor.parent->info.datatype.genericStateType;
  right = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
  right->tag = TYPE_PARAMETRIC_VAR;
  right->info.param_var = env;
  sig->domain =   st_MakeProdST_Type(left, right);

  sig->codomain = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
  sig->codomain->tag = TYPE_PARAMETRIC_VAR;
  sig->codomain->info.param_var = cod;

  return(sig);

}   /*  end st_MakeGenCombTypeSig()  */


/*********************************
 *                               *
 *    st_MakeGenericStateType    *
 *                               *
 *********************************/
ST_TYPE *
st_MakeGenericStateType(ST_ENTRY *dTypeEntry) {
/* relies on ST_PVAR being an int */

  ST_TYPE *gst     = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
  int      numParams = dTypeEntry->info.datatype.numParams,
           i;

  gst->tag = TYPE_USER_DATA;
  gst->info.user_data.name = dTypeEntry->name;
  gst->info.user_data.key = dTypeEntry->key;
  gst->info.user_data.args =
    (ST_TYPE **) MemHeapAlloc(symTab->scopeHD, numParams+1, sizeof(ST_ENTRY *));

  for (i=0; i<numParams; i++) {
    gst->info.user_data.args[i] =
      (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
    gst->info.user_data.args[i]->tag = TYPE_PARAMETRIC_VAR;
    gst->info.user_data.args[i]->info.param_var = i;
  }

  return(gst);

}


/*********************************
 *                               *
 *    st_MakeProdST_Type         *
 *                               *
 *********************************/
static ST_TYPE *
st_MakeProdST_Type(ST_TYPE *left, ST_TYPE *right)  {

  ST_TYPE *pType = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));

  pType->tag = TYPE_PROD;
  pType->info.prod.key = PROD_KEY;
  pType->info.prod.l = left;
  pType->info.prod.r = right;

  return(pType);

}


/*********************************
 *                               *
 *    st_ReplaceStateVar         *
 *                               *
 *********************************/
static ST_TYPE *
st_ReplaceStateVar(ST_TYPE *type, ST_TYPE *rplcmnt) {
/* copies type and replaces all occurences of state var with rplcmnt      */
/* the replacement should already exist in symTab->scopeHD, so we don't copy it here */

  ST_TYPE *newtype;
  int      i,
           numParams;

  switch (type->tag) {
  case TYPE_1              :

  case TYPE_BUILTIN_CHAR   :     /* [BI] HANDLE BUILTINS */
  case TYPE_BUILTIN_INT    :

  case TYPE_PARAMETRIC_VAR : newtype = type;    break;
  case TYPE_STATE_VAR      : newtype = rplcmnt; break;
  case TYPE_PROD           :
    if (st_ContainsStateVar(type)) {
      newtype = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
      newtype->tag = TYPE_PROD;
      newtype->info.prod.key = type->info.prod.key;
      newtype->info.prod.l = st_ReplaceStateVar(type->info.prod.l, rplcmnt);
      newtype->info.prod.r = st_ReplaceStateVar(type->info.prod.r, rplcmnt);
    }
    else
      newtype = type;
    break;
  case TYPE_USER_DATA :
    if (st_ContainsStateVar(type)) {
      numParams = st_GetNumParams(type->info.user_data.key);
      newtype = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
      newtype->tag = TYPE_USER_DATA;
      newtype->info.user_data.key = type->info.user_data.key;
      newtype->info.user_data.args =
        (ST_TYPE **)MHA(symTab->scopeHD, numParams+1, sizeof(ST_TYPE *));
      for (i=0; i<numParams; i++)
        newtype->info.user_data.args[i] =
          st_ReplaceStateVar(type->info.user_data.args[i], rplcmnt);
    }
    else
      newtype = type;
    break;
  default :
    printMsg(FATAL_MSG, "st_ReplaceStateVar() - invalid type tag %d.", type->tag);
  }

  return(newtype);

}


/*********************************
 *                               *
 *    st_ContansStateVar         *
 *                               *
 *********************************/
static BBOOL
st_ContainsStateVar(ST_TYPE *type) {

  int i,
      numParams;
  BBOOL result = BFALSE;

  switch (type->tag) {
  case TYPE_1              :

  case TYPE_BUILTIN_CHAR   :     /* [BI] HANDLE BUILTINS */
  case TYPE_BUILTIN_INT    :

  case TYPE_PARAMETRIC_VAR : result =  BFALSE;   break;
  case TYPE_STATE_VAR      : result =  BTRUE;    break;
  case TYPE_PROD           :
    result = (st_ContainsStateVar(type->info.prod.l) ||
              st_ContainsStateVar(type->info.prod.r));
    break;
  case TYPE_USER_DATA      :
    numParams = st_GetNumParams(type->info.user_data.key);
    for (i=0; i<numParams; i++)
      result = st_ContainsStateVar(type->info.user_data.args[i]) || result;
    break;
  default :
    printMsg(FATAL_MSG, "st_ContainsStateVar() - invalid type tag %d.", type->tag);
  }

  return(result);

}


/*********************************
 *                               *
 *    st_ChangeParamVars         *
 *                               *
 *********************************/
static ST_TYPE *
st_ChangeParamVars(ST_TYPE *type, int numParams) {
/* copies type and adds numParams to each of its parametric variables */

  ST_TYPE *newT;
  int      i;

  if (numParams == 0)
    return(type);

  newT = (ST_TYPE *)MHA(symTab->scopeHD, 1, sizeof(ST_TYPE));
  newT->tag = type->tag;
  switch (type->tag) {
  case TYPE_1              :
  case TYPE_STATE_VAR      :
    break;
  case TYPE_PARAMETRIC_VAR :
    newT->info.param_var = type->info.param_var+numParams;   break;
  case TYPE_PROD           :
    newT->info.prod.key = type->info.prod.key;
    newT->info.prod.l = st_ChangeParamVars(type->info.prod.l, numParams);
    newT->info.prod.r = st_ChangeParamVars(type->info.prod.r, numParams);
    break;
  case TYPE_USER_DATA      :
    newT->info.user_data.name = type->info.user_data.name;
    newT->info.user_data.key  = type->info.user_data.key;
    newT->info.user_data.args =
      (ST_TYPE **)MHA(symTab->scopeHD, numParams+1, sizeof(ST_TYPE *));
    for (i=0; i<numParams; i++)
        newT->info.user_data.args[i] =
          st_ChangeParamVars(type->info.user_data.args[i], numParams);
    break;
  default :
    printMsg(FATAL_MSG, "st_ChangeParamVars() - invalid type tag %d.", type->tag);
  }

  return(newT);

}

/************************************/
/*                                  */
/*  st_LinkMacroTypeSigsToFunction  */
/*                                  */
/************************************/
void st_LinkMacroTypeSigsToFunction(ST_KEY fnkey) {

    ST_ENTRY *entry, *macentry;
    int numMacros, i;

    entry = st_GetEntry(fnkey, NULL);

    if (entry->tag == ST_FUNCTION) {
        numMacros = entry->info.function.numMacros;
        if (numMacros > 0) {
            for (i=0; i<numMacros; i++) {
                macentry = st_GetEntry(entry->info.function.macroKeys[i],NULL);
                entry->info.function.type_sig->params[i] = macentry->info.macros.type_sig;
            }
        }
    }
    else printMsg(FATAL_MSG,"st_LinkMacroTypeSigsToFunction: not a function key");
}


/**********************
 *                    *
 *  st_UpdateTypeSig  *
 *                    *
 **********************/
void st_UpdateTypeSig(ST_KEY key, ST_TYPE_SIG *newsig) {
/* given a function or macro key, replace its current type sig by */
/*  a brand new copy of newsig                                    */

    ST_ENTRY *entry;

    entry = st_GetEntry(key, NULL);
    assert(entry);
    switch (entry->tag) {
    case ST_FUNCTION:
        entry->info.function.type_sig = st_CopyTypeSig(newsig,entry->info.function.numMacros);
        break;
    case ST_MACRO:
        entry->info.macros.type_sig = st_CopyTypeSig(newsig,0);  /* macros have 0 params */
        break;
    default:
        printMsg(FATAL_MSG,"st_UpdateTypeSig: key is not for function or macro");
    }
}


/********************
 *                  *
 *  st_CopyTypeSig  *
 *                  *
 ********************/
ST_TYPE_SIG *st_CopyTypeSig(ST_TYPE_SIG *sig, int numParams) {
/* given a type sig for a function or macro, build a new copy of it in st memory */
/* numParams is the size of the sig->params array */

    ST_TYPE_SIG *newsig;
    int i;

    if (!sig) return 0;
    newsig = (ST_TYPE_SIG *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE_SIG));
    newsig->domain = st_CopyType(sig->domain);
    newsig->codomain = st_CopyType(sig->codomain);
    if (numParams == 0) newsig->params = 0;
    else {
        newsig->params = (ST_TYPE_SIG **) MHA(symTab->scopeHD,numParams+1,sizeof(ST_TYPE_SIG *));
        for (i=0; i<numParams; i++) {
            newsig->params[i] = st_CopyTypeSig(sig->params[i],0);
        }
    }
    return newsig;
}

/*****************
 *               *
 *  st_CopyType  *
 *               *
 *****************/
ST_TYPE *st_CopyType(ST_TYPE *type) {
/* given an ST_TYPE, build a new copy of it in local st memory */

    ST_TYPE *newtype;
    int numParams, i;

    if (!type) return 0;
    newtype = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
    newtype->tag = type->tag;
    switch (type->tag) {
    case TYPE_1:
        break;
    case TYPE_PROD:
        newtype->info.prod.key = type->info.prod.key;
        newtype->info.prod.l = st_CopyType(type->info.prod.l);
        newtype->info.prod.r = st_CopyType(type->info.prod.r);
        break;
    case TYPE_PARAMETRIC_VAR:
        newtype->info.param_var = type->info.param_var;
        break;
    case TYPE_STATE_VAR:
        break;
    case TYPE_USER_DATA:
        newtype->info.user_data.name = type->info.user_data.name;
        newtype->info.user_data.key = type->info.user_data.key;
        numParams = st_GetNumParams(type->info.user_data.key);
        if (numParams == 0) newtype->info.user_data.args = 0;
        else {
            newtype->info.user_data.args = (ST_TYPE **) MemHeapAlloc(symTab->scopeHD,
                                                                     numParams+1,
                                                                     sizeof(ST_TYPE *));
            for (i=0; i<numParams; i++) {
                newtype->info.user_data.args[i] = st_CopyType(type->info.user_data.args[i]);
            }
        }
        break;

      case TYPE_BUILTIN_INT:      /* [BI] COPY BUILTINS */
      case TYPE_BUILTIN_CHAR:
        break;

    default:
        printMsg(FATAL_MSG,"st_CopyType: unknown tag field in st type");
    }
    return newtype;
}
