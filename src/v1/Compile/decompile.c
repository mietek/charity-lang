/******************************************************************************
 *                                                                            *
 *   decompile.c                                                              *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "types.h"
#include "lib.h"
#include "symtab.h"
#include "decompile.h"
#include "ctTranslate.h"
#include "pmem.h"
#include "ioChar.h"

#define MAX_DECOMPILE_SIZE 2000
#define MAX_CLO_TAB_SIZE   10000

/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/
static COMB_EXPR     *makeRecord(V_INSTR *rec, ST_TYPE *type);
static COMB_EXPR     *makeClosure(V_INSTR *clo, V_INSTR *record,
                                  ST_TYPE *type);

static CLO_TAB_ENTRY  CloTabEntryNew(V_INSTR *clo, V_INSTR *record,
                                     ST_TYPE *type);

/*****************************************************************************
 *                                                                           *
 *                           internal variables                              *
 *                                                                           *
 *****************************************************************************/
static MEMORY     decompHD     = -1;
static MEMORY     cloTabHD     = -1;
static int        cloTabIdx    = 0;
static CLOSURE   *cloTab       = NULL;  /* The is an array of closures */
static BBOOL      rightDisplay = BFALSE;

/***************************************
 *                                     *
 *       deCompileConstruct            *
 *                                     *
 ***************************************/
void
deCompileConstruct(void)
{
  decompHD  = MemAlloc("decompile", MAX_DECOMPILE_SIZE, sizeof(COMB_EXPR));
  cloTabHD  = MemAlloc("decompile closure", MAX_CLO_TAB_SIZE, sizeof(CLOSURE));
  cloTab    = (CLOSURE *) MemHeapAlloc(cloTabHD, MAX_CLO_TAB_SIZE,
                                       sizeof(CLOSURE));
  cloTabIdx = 0;
}

/***************************************
 *                                     *
 *       deCompileDestruct             *
 *                                     *
 ***************************************/
void
deCompileDestruct(void)
{
  MemDealloc(decompHD);
  MemDealloc(cloTabHD);
}

/***************************************
 *                                     *
 *       deCompileReset                *
 *                                     *
 ***************************************/
void
deCompileReset(void)
{
  MemReset(decompHD);
  MemReset(cloTabHD);
  cloTabIdx = 0;
  rightDisplay = BFALSE;
}

/***************************************
 *                                     *
 *       isInRightDisplay              *
 *                                     *
 ***************************************/
BBOOL
isInRightDisplay(void)
{
  return(rightDisplay);
}

/***************************************
 *                                     *
 *       getClosure                    *
 *                                     *
 ***************************************/
CLOSURE
*getClosure(CLO_TAB_ENTRY clo_entry)
{
  CLOSURE *pages = NULL;

  pages = (CLOSURE *) getPage(cloTabHD, (clo_entry / MAX_CLO_TAB_SIZE) + 1);
  return(&pages[clo_entry % MAX_CLO_TAB_SIZE]);
}

/***************************************
 *                                     *
 *       getNumClosures                *
 *                                     *
 ***************************************/
int
getNumClosures(void)
{
  int pages = getNumPages(cloTabHD);

  if (pages == 1) {
    return(cloTabIdx);
  }
  else {
    return(cloTabIdx + (MAX_CLO_TAB_SIZE * (pages - 1)));
  }
}

/***************************************
 *                                     *
 *       CloTabEntryNew                *
 *                                     *
 ***************************************/
static
CLO_TAB_ENTRY
CloTabEntryNew(V_INSTR *clo, V_INSTR *record, ST_TYPE *type)
{
  if (cloTabIdx >= MAX_CLO_TAB_SIZE - 2) {
    cloTab = (CLOSURE *) MemHeapAlloc(cloTabHD, MAX_CLO_TAB_SIZE,
                                      sizeof(CLOSURE));
    cloTabIdx = 0;
  }

  cloTab[cloTabIdx].rec        = record;
  cloTab[cloTabIdx].clo        = clo;
  cloTab[cloTabIdx].type       = type;

  return(cloTabIdx++);
}

/***************************************
 *                                     *
 *       makeClosure                   *
 *                                     *
 ***************************************/
static
COMB_EXPR
*makeClosure(V_INSTR *clo, V_INSTR *record, ST_TYPE *type)
{
  COMB_EXPR *result  = NULL;

  result = (COMB_EXPR *) MemHeapAlloc(decompHD, 1, sizeof(COMB_EXPR));
  assert(result);

  rightDisplay         = BTRUE;

  result->tag          = CTT_CLOSURE;
  result->info.closure = CloTabEntryNew(clo, record, type);
  return(result);
}

/***************************************
 *                                     *
 *            makeRecord               *
 *                                     *
 ***************************************/
static
COMB_EXPR
*makeRecord(V_INSTR *rec, ST_TYPE *type)
{
  COMB_EXPR  *result     = NULL;
  char       *parentName = NULL;
  char        str[MAX_STRING_LENGTH_DEFAULT];
  int         count      = 0;
  char       **destrs    = NULL;
  ST_TYPE    *cloType    = NULL;

  assert(rec);

  result = (COMB_EXPR *) MemHeapAlloc(decompHD, 1, sizeof(COMB_EXPR));
  assert(result);

  result->tag = CTT_COMBINATOR;
  result->info.combinator.class = CC_RECORD;

  parentName = type->info.user_data.name;

  result->info.combinator.parentName = parentName;

  strcpy(str, "record_");
  strcat(str, parentName);

  result->info.combinator.name      = libStrdup(decompHD, str);
  result->info.combinator.numParams = getNumStructors(parentName);

  result->info.combinator.param =
    (COMB_PHR **) MemHeapAlloc(decompHD, result->info.combinator.numParams,
                               sizeof(COMB_PHR *));

  assert(result->info.combinator.numParams < 50);

  destrs = getStructorNames(parentName);

  /* ignore 1st dummy record which holds macro frame */
  for (count = 0; count < result->info.combinator.numParams; count++) {
    cloType = st_SubstType(decompHD,
                           getStructorType(destrs[count]),
                           type,
                           type->info.user_data.args);

    result->info.combinator.param[count] =
    (COMB_PHR *) MemHeapAlloc(decompHD, 1, sizeof(COMB_PHR));

    result->info.combinator.param[count]->positive = makeClosure(&rec[count+1], rec,
                                                                 cloType);
  }

  return(result);
}

/***************************************
 *                                     *
 *            deCompile                *
 *                                     *
 ***************************************/
COMB_EXPR
*deCompile(V_INSTR *value, ST_TYPE *type)
{
  COMB_EXPR  *result           = NULL;
  char      **constructorArray = NULL;
  ST_TYPE    *newType          = NULL;

  if (!value || value == _BANG)
    return(CombExprNew(decompHD, CC_PRIMITIVE, "!", 0, NULL));

  switch (type->tag) {
  case TYPE_1:
    V_assert(value == _BANG);
    result =  CombExprNew(decompHD, CC_PRIMITIVE, "!", 0, NULL);
    break;
  case TYPE_PROD:
    V_assert(value->instr == MCldmacroframe);
    result = CombExprPair(decompHD,
                          deCompile(value->info.pair.v0, type->info.prod.l),
                          deCompile(value->info.pair.v1, type->info.prod.r));
    break;
  case TYPE_USER_DATA:
    if (value->info.gc.gcId >= 1) {     /* [#@] */
      constructorArray = getStructorNames(type->info.user_data.name);

      assert(constructorArray[value->info.structor.posn - 1]);     /* [#@] */
      result = CombExprNew(decompHD,
                           CC_CONSTRUCTOR,
                           constructorArray[value->info.structor.posn - 1],     /* [#@] */
                           0,
                           NULL);

      if (value->info.structor.next) {
        newType = st_SubstType(decompHD,
                               getStructorType(constructorArray[value->info.structor.posn - 1]),     /* [#@] */
                               type,
                               type->info.user_data.args);
        result = CombExprComposition(decompHD, result,
                                     deCompile(value->info.structor.next,
                                               newType));
      }
    }
    else
      result = makeRecord(value, type);
    break;

  case TYPE_BUILTIN_INT:                                             /* [BI] DECOMPILE BUILTIN INTEGERS   */
    result = CombInt (decompHD, value->info.integer.i);

    break;

  case TYPE_BUILTIN_CHAR:
    result = CombChar (decompHD, (char)value->info.character.c);     /* [BI] DECOMPILE BUILTIN CHARACTERS */

    break;

  case TYPE_PARAMETRIC_VAR:
  case TYPE_STATE_VAR:
  default:
    printMsg(FATAL_MSG, "deCompile - Invalid type");
    break;
  }
  return(result);
}
