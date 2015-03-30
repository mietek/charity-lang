/******************************************************************************
 *                                                                            *
 *   lookupSymtab.c                                                           *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <string.h>

#include "symtab.h"
#include "symtabI.h"
#include "ioChar.h"
#include "variance.h"     /* [H-O] ADDED THIS INCLUDE */

/*************************************************
 *                                               *
 * Prototypes For Private Symbol Table Functions *
 *                                               *
 *************************************************/

/* lookup functions */
static char   *makeMacroName(char *funName,
                             char *macroName,
                             char *fullMacroName);

/****************************************************************************
 *                                                                          *
 *    Lookup Function Definitions                                           *
 *                                                                          *
 ****************************************************************************/


/*********************************
 *                               *
 *    st_DumpTable               *
 *                               *
 *********************************/

/* TOPMOST SCOPE ONLY */

void
st_DumpTable (void)
{
  ST_ENTRY *nextEntry;
  int i;

  for (i=0;i<SYMTAB_SIZE;i++) {
    nextEntry = symTab->table[i];
    while (nextEntry) {
      switch (nextEntry->tag) {
      case ST_FUNCTION:
      case ST_DATATYPE:
      case ST_STRUCTOR:
      case ST_OPCOMB:
      case ST_VAR:
        st_PrintEntryInfo(nextEntry->key);
        break;
      case ST_MACRO:
        /* don't show these, already in function printout */
        break;
      default:
        printMsg(FATAL_MSG, "st_DumpTable() - invalid tag %d.",nextEntry->tag);
      }   /*  hctiws  */
      nextEntry = nextEntry->nextEntry;
    }   /*  elihw  */
  }   /* rof */

}   /*  end st_DumpTable  */


/*********************************
 *                               *
 *    st_ShowDatatypeCombinators *
 *                               *
 *********************************/
void
st_ShowDatatypeCombinators(ST_KEY dataKey) {

  ST_ENTRY *dataEntry = st_GetEntry(dataKey, NULL);
  int i;

  for (i=0;i<3;i++)
    st_PrintEntryInfo(dataEntry->info.datatype.opCombKeys[i]);

}


/*********************************
 *                               *
 *    getStructorNames           *
 *                               *
 *********************************/
char **
getStructorNames(char *typeName) {

  ST_ENTRY  *entry;

  entry = getEntry(typeName);
  assert(entry);
  return(entry->info.datatype.structorNames);

}


/*********************************
 *                               *
 *    st_GetStructorNames        *
 *                               *
 *********************************/
char **
st_GetStructorNames(ST_KEY typeKey) {

  ST_ENTRY  *entry;

  entry = st_GetEntry(typeKey, NULL);
  assert(entry);

  return entry->info.datatype.structorNames;

}


/*********************************
 *                               *
 *    st_GetStructorKeys         *
 *                               *
 *********************************/

ST_KEY *
st_GetStructorKeys (ST_KEY typeKey)
{
  ST_ENTRY *entry = st_GetEntry (typeKey, NULL);

  assert (entry);

  return entry->info.datatype.structorKeys;
}


/*********************************
 *                               *
 *    getNumStructors            *
 *                               *
 *********************************/

int
getNumStructors (char *typeName)
{
  ST_ENTRY *entry = getEntry (typeName);

  assert (entry);

  return entry->info.datatype.numStructors;
}


/*********************************
 *                               *
 *    st_GetNumStructors         *
 *                               *
 *********************************/

int
st_GetNumStructors (ST_KEY typeKey)
{
  ST_ENTRY *entry = st_GetEntry (typeKey, NULL);

  assert (entry);

  return entry->info.datatype.numStructors;
}


/*********************************
 *                               *
 *    st_GetStructorPosn         *
 *                               *
 *********************************/

int
st_GetStructorPosn (ST_KEY structKey)
{
  ST_ENTRY *entry = st_GetEntry (structKey, NULL);

  if (entry->tag != ST_STRUCTOR)
    printMsg(FATAL_MSG, "st_GetStructorPosn() - invalid tag %d.", entry->tag);

  return entry->info.structor.posn;
}


/*********************************
 *                               *
 *    getStructorPosn            *
 *                               *
 *********************************/

/* RETURNS -1 ON ERROR */

int
getStructorPosn(char *structorName)
{
  ST_ENTRY *entry = getEntry (structorName);

  if (entry)
    return entry->info.structor.posn;
  else
    return -1;
}


/*********************************
 *                               *
 *    getTypeParams              *
 *                               *
 *********************************/

int
getTypeParams (char *typeName)
{
  ST_ENTRY *entry = getEntry (typeName);

  assert (entry);

  switch (entry->tag)
    {
    case ST_DATATYPE:
      return entry->info.datatype.numParams;

    case ST_ALIAS:
      return entry->info.alias.numParams;

    default:
      assert (BFALSE);
    }
}


/*********************************
 *                               *
 *    getAliasExpansion          *
 *                               *
 *********************************/

ST_TYPE *
getAliasExpansion (char *alias)
{
  ST_ENTRY *entry = NULL;

  assert (alias);

  entry = getEntry (alias);

  assert (entry);
  assert (entry->tag == ST_ALIAS);
  assert (entry->info.alias.expansion);

  return entry->info.alias.expansion;
}


/*********************************
 *                               *
 *    getAliasNumParams          *
 *                               *
 *********************************/

int
getAliasNumParams (char *alias)
{
  ST_ENTRY *entry = NULL;

  assert (alias);

  entry = getEntry (alias);

  assert (entry);
  assert (entry->tag == ST_ALIAS);
  assert (entry->info.alias.numParams >= 0);

  return entry->info.alias.numParams;
}


/*********************************
 *                               *
 *    getAliasVarity             *
 *                               *
 *********************************/

V_VARIANCE *
getAliasVarity (char *alias)
{
  ST_ENTRY *entry = NULL;

  assert (alias);

  entry = getEntry (alias);

  assert (entry);
  assert (entry->tag == ST_ALIAS);
  assert (entry->info.alias.numParams ? entry->info.alias.varity != NULL : entry->info.alias.varity == NULL);

  return entry->info.alias.varity;
}


/*********************************
 *                               *
 *    getNumParams               *
 *                               *
 *********************************/

int
getNumParams(char *typeName)
{
  return getTypeParams (typeName);
}


/*********************************
 *                               *
 *    st_GetVarity               *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION: */

V_VARIANCE *
st_GetVarity (ST_KEY typeKey)
{
  ST_ENTRY *entry = st_GetEntry (typeKey, NULL);

  assert (entry);

  return (entry->info.datatype.varity);
}


/*********************************
 *                               *
 *    st_IsHO                    *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION: */

BBOOL
st_IsHO (ST_KEY destKey)
{
  ST_ENTRY *entry = st_GetEntry (destKey, NULL);

  assert (entry);

  if (entry->tag == ST_STRUCTOR)
    return entry->info.structor.isHO;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    st_GetNumParams            *
 *                               *
 *********************************/

int
st_GetNumParams (ST_KEY typeKey)
{
  ST_ENTRY *entry = st_GetEntry (typeKey, NULL);

  return entry->info.datatype.numParams;
}


/*********************************
 *                               *
 *    isInductiveType            *
 *                               *
 *********************************/

BBOOL
isInductiveType (char *typeName)
{
  ST_ENTRY *entry = getEntry(typeName);

  if (!entry)
    return BFALSE;

  if (!isDatatype (typeName))
    return BFALSE;

  if (entry->info.datatype.class == DT_INDUCTIVE)
    return BTRUE;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    st_IsInductiveType         *
 *                               *
 *********************************/

BBOOL
st_IsInductiveType (ST_KEY typeKey)
{
  ST_ENTRY *entry = st_GetEntry (typeKey, NULL);

  /* [FIX] STRING VERSION BEHAVES SLIGHTLY DIFFERENTLY */

  if (entry->info.datatype.class == DT_INDUCTIVE)
    return BTRUE;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    isCoinductiveType          *
 *                               *
 *********************************/

BBOOL
isCoinductiveType (char *typeName)
{
  return !isInductiveType (typeName);
}


/*********************************
 *                               *
 *    st_IsCoinductiveType       *
 *                               *
 *********************************/

BBOOL
st_IsCoinductiveType (ST_KEY typeKey)
{
  return !st_IsInductiveType (typeKey);
}


/*********************************
 *                               *
 *    st_GetStructorParent       *
 *                               *
 *********************************/

ST_KEY
st_GetStructorParent (ST_KEY structKey)
{
  ST_ENTRY *entry = st_GetEntry (structKey, NULL);

  if (entry->tag != ST_STRUCTOR)
    printMsg(FATAL_MSG,"st_GetStructorParent() - invalid tag %d.", entry->tag);

  return entry->info.structor.parent->key;
}


/*********************************
 *                               *
 *    getStructorParent          *
 *                               *
 *********************************/

char *
getStructorParent (char *structorName)
{
  ST_ENTRY *entry = getEntry (structorName);

  assert (entry);

  return entry->info.structor.parent->name;
}


/*********************************
 *                               *
 *    isConstructor              *
 *                               *
 *********************************/

BBOOL
isConstructor (char *structorName)
{
  ST_ENTRY *entry = getEntry (structorName);

  if (!entry)
    return BFALSE;

  if (!isStructor (structorName))
    return BFALSE;

  entry = entry->info.structor.parent;

  return isInductiveType (entry->name);
}


/*********************************
 *                               *
 *    st_IsConstructor           *
 *                               *
 *********************************/

BBOOL
st_IsConstructor (ST_KEY structKey)
{
  ST_ENTRY *entry = st_GetEntry(structKey, NULL);

  /* [FIX] STRING VERSION BEHAVES SLIGHTLY DIFFERENTLY */

  entry = entry->info.structor.parent;

  return st_IsInductiveType (entry->key);
}


/*********************************
 *                               *
 *    isDestructor               *
 *                               *
 *********************************/

BBOOL
isDestructor (char *structorName)
{
  return !isConstructor (structorName);
}


/*********************************
 *                               *
 *    st_IsDestructor            *
 *                               *
 *********************************/

BBOOL
st_IsDestructor (ST_KEY structKey)
{
  ST_ENTRY *entry = st_GetEntry(structKey, NULL);

  /* [FIX] STRING VERSION BEHAVES SLIGHTLY DIFFERENTLY */

  entry = entry->info.structor.parent;

  return st_IsCoinductiveType(entry->key);
}


/*********************************
 *                               *
 *    getMacroNames              *
 *                               *
 *********************************/

char **
getMacroNames (char *funName)
{
  ST_ENTRY *entry = getEntry (funName);

  assert (entry);

  return entry->info.function.macroNames;
}


/*********************************
 *                               *
 *    st_GetMacroPosn            *
 *                               *
 *********************************/

int
st_GetMacroPosn (ST_KEY macroKey)
{
  ST_ENTRY *entry = st_GetEntry(macroKey, NULL);

  if (entry->tag == ST_MACRO)
    return entry->info.macros.posn;
  else
    printMsg (FATAL_MSG, "st_GetMacroPosn() - invalid tag %d", entry->tag);
}


/*********************************
 *                               *
 *    st_GetMacroKeys            *
 *                               *
 *********************************/

ST_KEY *
st_GetMacroKeys (ST_KEY funKey)
{
  ST_ENTRY *entry = st_GetEntry (funKey, NULL);

  assert (entry);

  if (entry->tag == ST_FUNCTION)
    return entry->info.function.macroKeys;
  else
    printMsg (FATAL_MSG, "st_GetMacroKeys() - invalid tag %d", entry->tag);
}


/*********************************
 *                               *
 *    getNumMacros               *
 *                               *
 *********************************/

int
getNumMacros (char *funName)
{
  ST_ENTRY *entry = getEntry (funName);

  assert (entry);

  return entry->info.function.numMacros;
}


/*********************************
 *                               *
 *    st_GetInstruction          *
 *                               *
 *********************************/

M_INSTR_TAG
st_GetInstruction (ST_KEY functionKey)
{
  ST_ENTRY *entry = NULL;

  assert (functionKey);

  entry = st_GetEntry (functionKey, NULL);

  assert (entry);
  assert (entry->tag == ST_FUNCTION);

  return entry->info.function.instr;
}


/*********************************
 *                               *
 *    st_GetNumMacros            *
 *                               *
 *********************************/
int
st_GetNumMacros(ST_KEY funKey) {

  ST_ENTRY  *entry;

  entry = st_GetEntry(funKey, NULL);
  assert(entry);
  if (entry->tag == ST_FUNCTION)
    return(entry->info.function.numMacros);
  else
    printMsg(FATAL_MSG, "st_GetNumMacros() - invalid tag %d", entry->tag);

}


/*********************************
 *                               *
 *    st_GetTypeSig              *
 *                               *
 *********************************/

ST_TYPE_SIG *
st_GetTypeSig (ST_KEY key)
{
  ST_ENTRY *entry = st_GetEntry (key, NULL);

  assert (entry);

  switch (entry->tag)
    {
    case ST_FUNCTION : return entry->info.function.type_sig;
    case ST_MACRO    : return entry->info.macros.type_sig;
    case ST_STRUCTOR : return entry->info.structor.type_sig;
    case ST_OPCOMB   : return entry->info.opcomb.type_sig;

    case ST_VAR      :
    case ST_DATATYPE :
        printMsg(ERROR_MSG, "Can't get type signature for variables or datatypes.");
        break;
    default          :
      printMsg (FATAL_MSG, "st_GetTypeSig() invalid tag %d\n", entry->tag);
    }
}


/*********************************
 *                               *
 *   st_GetMacroTypeSigs         *
 *                               *
 *********************************/

ST_TYPE_SIG **
st_GetMacroTypeSigs (ST_KEY funKey)
{
  ST_ENTRY *entry = st_GetEntry (funKey, NULL);

  if (entry->tag != ST_FUNCTION)
    printMsg (FATAL_MSG, "st_GetMacroTypeSigs() - invalid tag %d", entry->tag);

  return entry->info.function.type_sig->params;
}


/*********************************
 *                               *
 *    getStructorTypeSig         *
 *                               *
 *********************************/

ST_TYPE_SIG *
getStructorTypeSig (char *structorName)
{
  ST_ENTRY *entry = getEntry (structorName);

  assert (entry);

  return entry->info.structor.type_sig;
}


/*********************************
 *                               *
 *    getFunTypeSig              *
 *                               *
 *********************************/

ST_TYPE_SIG *
getFunTypeSig (char *funName)
{
  ST_ENTRY *entry = getEntry (funName);

  assert (entry);

  return entry->info.function.type_sig;
}


/*********************************
 *                               *
 *    getStructorType            *
 *                               *
 *********************************/

ST_TYPE *
getStructorType (char *structorName)
{
  ST_TYPE_SIG *typeSig = getStructorTypeSig (structorName);

  if (isConstructor (structorName))
    return typeSig->domain;
  else
    return typeSig->codomain;
}


/***************************************
 *                                     *
 *            st_SubstType             *
 *                                     *
 ***************************************/
ST_TYPE
*st_SubstType(MEMORY heap, ST_TYPE *type, ST_TYPE *state, ST_TYPE **parms)
{
/* Given a type, substitute the state variables and parametric variables
 * eg: list(A * C), where A is a parametric variable and C is a state variable,
 *                  and we want to replace A with bool and C with bush(bool)
 * --> list(bool * bush(bool))
 */
     ST_TYPE *result = NULL;
     int      count  = 0;
     int      num    = 0;

     assert(type);

     switch (type->tag) {
        case TYPE_1:
          result = (ST_TYPE *) MemHeapAlloc(heap, 1, sizeof(ST_TYPE));
          assert(result);
          result->tag = TYPE_1;
          break;

        /* [BI] ADDED: */

        case TYPE_BUILTIN_CHAR:
          result = (ST_TYPE *)MHA (heap, 1, sizeof (ST_TYPE));

          assert (result);

          result->tag = TYPE_BUILTIN_CHAR;

          break;

        case TYPE_BUILTIN_INT:
          result = (ST_TYPE *)MHA (heap, 1, sizeof (ST_TYPE));

          assert (result);

          result->tag = TYPE_BUILTIN_INT;

          break;

        case TYPE_PROD:
          result = (ST_TYPE *) MemHeapAlloc(heap, 1, sizeof(ST_TYPE));
          assert(result);
          result->tag = TYPE_PROD;
          result->info.prod.key = type->info.prod.key;
          result->info.prod.l = st_SubstType(heap, type->info.prod.l, state, parms);
          result->info.prod.r = st_SubstType(heap, type->info.prod.r, state, parms);
          break;
        case TYPE_PARAMETRIC_VAR:
          result = parms[type->info.parametric_var];
          break;
        case TYPE_STATE_VAR:
          result = state;
          break;
        case TYPE_USER_DATA:
          result = (ST_TYPE *) MemHeapAlloc(heap, 1, sizeof(ST_TYPE));
          assert(result);
          result->tag = TYPE_USER_DATA;
          result->info.user_data.name = type->info.user_data.name;
          result->info.user_data.key  = type->info.user_data.key;
          num = st_GetNumParams(type->info.user_data.key);
          result->info.user_data.args = (ST_TYPE **) MemHeapAlloc(heap,
                                                                  num+1, sizeof(ST_TYPE *));
          assert(result->info.user_data.args);
          for (count = 0; count < num; count++) {
               result->info.user_data.args[count] = st_SubstType(heap,
                                                                 type->info.user_data.args[count],
                                                                 state, parms);
          }

          break;
        default:
          /* should never reach this point */
          printMsg(FATAL_MSG, "st_SubstType, unknown tag");
     }

     return(result);
}


/*********************************
 *                               *
 *    isFunction                 *
 *                               *
 *********************************/

BBOOL
isFunction (char *name)
{
  ST_ENTRY *entry = getEntry (name);

  if (!entry)
    return BFALSE;

  if (entry->tag == ST_FUNCTION)
    return BTRUE;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    isDatatype                 *
 *                               *
 *********************************/

BBOOL
isDatatype (char *name)
{
  ST_ENTRY *entry = getEntry (name);

  if (!entry)
    return BFALSE;

  if (entry->tag == ST_DATATYPE)
    return BTRUE;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    isAlias                    *
 *                               *
 *********************************/

BBOOL
isAlias (char *name)
{
  ST_ENTRY *entry = getEntry (name);

  if (!entry)
    return BFALSE;

  if (entry->tag == ST_ALIAS)
    return BTRUE;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    isStructor                 *
 *                               *
 *********************************/

BBOOL
isStructor (char *name)
{
  ST_ENTRY *entry = getEntry (name);

  if (!entry)
    return BFALSE;

  if (entry->tag == ST_STRUCTOR)
    return BTRUE;
  else
    return BFALSE;
}


/*******************
 *                 *
 *  st_IsStructor  *
 *                 *
 *******************/
BBOOL
st_IsStructor(ST_KEY key) {

  ST_ENTRY *entry = st_GetEntry(key, NULL);

  if (entry->tag == ST_STRUCTOR)
    return(BTRUE);
  else
    return(BFALSE);

}


/****************
 *              *
 *  st_IsMacro  *
 *              *
 ****************/

BBOOL st_IsMacro (ST_KEY key)
{
  ST_ENTRY *entry = st_GetEntry (key, NULL);

  assert (entry);

  if (entry->tag == ST_MACRO)
    return BTRUE;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    st_IsMacroByName           *
 *                               *
 *********************************/

BBOOL
st_IsMacroByName (char *macroName)
{
  ST_ENTRY *entry = st_GetEntry (st_NameToKey (macroName), NULL);

  if (entry && entry->tag == ST_MACRO)
    return BTRUE;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    isMacro                    *
 *                               *
 *********************************/

BBOOL
isMacro (char *funName,
         char *name)
{
  int       totLen = strlen (funName) + strlen (name) + strlen (RES_PREFIX);

  char     *fullName;
  MEMORY    strHeapDesc;
  ST_ENTRY *entry;

  strHeapDesc = MemAlloc("symtab string", sizeof(char), totLen+2);
  fullName = (char *)MemHeapAlloc(symTab->scopeHD, totLen+2, sizeof(char));
  fullName = makeMacroName(funName, name, fullName);

  entry = getEntry (fullName);

  MemDealloc (strHeapDesc);

  if (!(entry)) return(BFALSE);
  if (entry->tag == ST_MACRO)
    return(BTRUE);
  else
    return(BFALSE);
}


/*********************************
 *                               *
 *    makeMacroName              *
 *                               *
 *********************************/
char *
makeMacroName(char *funName, char *macroName, char *fullMacroName) {

strcat(fullMacroName, RES_PREFIX);
strcat(fullMacroName, funName);
strcat(fullMacroName, ".");
strcat(fullMacroName, macroName);
return(fullMacroName);

}


/*********************************
 *                               *
 *    st_GetParamPosn            *
 *                               *
 *********************************/
int
st_GetParamPosn(ST_PVAR param) {

  return ((int)param);

}


/********************************
 *                              *
 *     st_NameToKey             *
 *                              *
/********************************/
ST_KEY
st_NameToKey(char *name) {

  ST_ENTRY *entry = getEntry(name);

  if (entry)
    return(entry->key);
  else
    return((ST_KEY)NULL);

}


/********************************
 *                              *
 *     st_KeyToName             *
 *                              *
/********************************/
char *
st_KeyToName(ST_KEY key)  {
/* returns NULL on fail */

  ST_ENTRY *entry = st_GetEntry(key, NULL);

  if (entry)
    return(entry->name);
  else
    return((char *)NULL);

}


/****************************
 *                          *
 *  st_GetGenericStateType  *
 *                          *
 ****************************/
ST_TYPE *
st_GetGenericStateType(ST_KEY key) {

  ST_ENTRY *entry       = st_GetEntry(key, NULL);

  assert(entry);

  switch(entry->tag) {
    case ST_STRUCTOR: return(entry->info.structor.parent->info.datatype.genericStateType);
    default: printMsg(FATAL_MSG,"st_GetGenericStateType() - argument not a structor");
  }

}


/********************************
 *                              *
 *     st_GetDatatypeTag        *
 *                              *
/********************************/
ST_TYPE_TAG
st_GetDatatypeTag(ST_KEY typeKey) {

  if (typeKey == PROD_KEY)
    return(TYPE_PROD);
  else if (typeKey == TERM_KEY)
    return(TYPE_1);
  else
    return(TYPE_USER_DATA);

}


/*********************************
 *                               *
 *    st_IsFunction              *
 *                               *
 *********************************/
BBOOL
st_IsCombinator(ST_KEY key) {

  ST_ENTRY  *entry;

  entry = st_GetEntry(key, NULL);
  if (!(entry)) return(BFALSE);
  if (entry->tag == ST_OPCOMB)
    return(BTRUE);
  else
    return(BFALSE);

}


/*********************************
 *                               *
 *    st_IsFunction              *
 *                               *
 *********************************/
BBOOL
st_IsFunction(ST_KEY key) {

  ST_ENTRY  *entry;

  entry = st_GetEntry(key, NULL);
  if (!(entry)) return(BFALSE);
  if (entry->tag == ST_FUNCTION)
    return(BTRUE);
  else
    return(BFALSE);

}


/*********************************
 *                               *
 *    st_IsDatatype              *
 *                               *
 *********************************/
BBOOL
st_IsDatatype(ST_KEY key) {

  ST_ENTRY  *entry;

  entry = st_GetEntry(key, NULL);
  if (!(entry)) return(BFALSE);
  if (entry->tag == ST_DATATYPE)
    return(BTRUE);
  else
    return(BFALSE);

}


/*********************************
 *                               *
 *    st_GetOpCombParentByName   *
 *                               *
 *********************************/
char *
st_GetOpCombParentByName(char *combName) {

  ST_ENTRY *entry = st_GetEntry(st_NameToKey(combName), NULL);

  return(entry->info.opcomb.parent->name);

}


/*********************************
 *                               *
 *    st_GetNumDistinctParams    *
 *                               *
 *********************************/
int
st_GetNumDistinctParams(ST_TYPE_SIG *sig) {

  return(2);

}


/*******************
 *                 *
 *  st_IsVar       *
 *                 *
 *******************/
BBOOL
st_IsVar(ST_KEY key, int *level) {

  ST_ENTRY *entry = st_GetEntry(key,level);

  return (entry->tag == ST_VAR ? BTRUE : BFALSE);

}   /*  end st_IsVar  */


/*******************
 *                 *
 *  st_IsHOVar     *
 *                 *
 *******************/
BBOOL
st_IsHOVar(ST_KEY key, int *level) {

  ST_ENTRY *entry = st_GetEntry(key,level);
  if (entry->tag == ST_VAR)
    return (entry->info.var.isHO);
  else return BFALSE;
}


/********************
 *                  *
 *  st_GetUniqueVar *
 *                  *
 ********************/
char *
st_GetUniqueVar(ST_KEY varKey) {

  ST_ENTRY *entry = st_GetEntry(varKey, NULL);

  return entry->info.var.uniqueVar;

}   /*  end st_GetUniqueVar  */
