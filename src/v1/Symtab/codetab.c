/******************************************************************************
 *                                                                            *
 *   codetab.c                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "ioChar.h"
#include "codetab.h"
#include "lib.h"
#include "pmem.h"
#include "ctTranslate.h"

#define TRANS_HEAP_SIZE   10000
#define CODETAB_SIZE      191    /* prime not too close to a power of 2 */

/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/
static int        _hashKey(char *key);
static CT_ENTRY  *_findEntry(char *name, CT_ENTRY *entry);
static CT_ENTRY  *_NewEntry(char *name, COMB_EXPR *expr, M_INSTR *mc);

int kludge;

/*****************************************************************************
 *                                                                           *
 *                           internal variables                              *
 *                                                                           *
 *****************************************************************************/


static MEMORY     codeTblDesc;      /* the "code table" eg: the hash table */
static CT_ENTRY **codeTable;

static MEMORY     codeHD;       /* the heap to hold individual entries */
static MEMORY     transHD;
/*********************************
 *                               *
 *    _hashKey                   *
 *                               *
 *********************************/
/* uses straightforward division algorithm for determining hash value */
/* duplicated from Symtab.c                                           */
static
int
_hashKey(char *key) {
  int keySize,
      i,
      total = 0,
      size = CODETAB_SIZE;

  keySize = strlen(key);
  for (i=0; i < keySize; i++) {
    total = total + (int)key[i];
  }
  return(total % size);

}

/*********************************
 *                               *
 *    CodeTableConstruct         *
 *                               *
 *********************************/
void
CodeTableConstruct(int size)
{
/* !!!! should use passed in size */
     codeTblDesc = MemAlloc("code table", CODETAB_SIZE, sizeof(CT_ENTRY *));
     codeTable   = (CT_ENTRY **) MemHeapAlloc(codeTblDesc,
					      CODETAB_SIZE,
					      sizeof(CT_ENTRY *));

     assert(codeTable);

     codeHD = MemAlloc("code heap", CODETAB_HEAP_SIZE, sizeof(CT_ENTRY));
     CompilerConstruct();
     transHD = MemAlloc("combinator heap", TRANS_HEAP_SIZE, sizeof(COMB_EXPR));
}

/*********************************
 *                               *
 *    CodeTableDestruct          *
 *                               *
 *********************************/
void
CodeTableDestruct(void)
{
     MemDealloc(codeTblDesc);
     MemDealloc(codeHD);
     MemDealloc(transHD);
     CompilerDestruct();
}

/*********************************
 *                               *
 *    CodeTableReset             *
 *                               *
 *********************************/
void
CodeTableReset(void)
{
     MemReset(codeTblDesc);
     MemReset(codeHD);     
     MemReset(transHD);

     ctTranslateReset();
     CompilerReset();
}


/*********************************
 *                               *
 *    _NewEntry                  *
 *                               *
 *********************************/
static
CT_ENTRY
*_NewEntry(char *name, COMB_EXPR *expr, M_INSTR *mc)
{
     CT_ENTRY *entry = (CT_ENTRY *) MemHeapAlloc(codeHD, 1, sizeof(CT_ENTRY));
     assert(name);
     assert(expr);
/*     assert(mc);   !!!! */
     assert(entry);

     entry->name         = libStrdup(codeHD, name);
/*     entry->comb_exp     = CombExprDuplicate(transHD, expr); */
     entry->comb_exp     = NULL;
     entry->machine_code = mc;
     entry->next         = NULL;
     
     return(entry);
}

/*********************************
 *                               *
 *    CodeTableAdd               *
 *                               *
 *********************************/
void
CodeTableAdd(char *name, COMB_EXPR *expr, M_INSTR *mc)
{
     CT_ENTRY *entry = NULL;
     CT_ENTRY *prev  = NULL;
     CT_ENTRY *curr  = NULL;
     int       hash  = -1;

     hash = _hashKey(name);
     if (codeTable[hash]) { /* entry exists in table, search for free space */
	  curr = prev = codeTable[hash];

	  while (curr && strcmp(curr->name, name) != 0) {
	       prev = curr;
	       curr = curr->next;
	  }
	  if (curr) { /* name already exists in table */
	       curr->comb_exp = expr;
	       curr->machine_code = mc;
	  }
	  else {
	       entry = _NewEntry(name, expr, mc);
	       assert(entry);
	       prev->next = entry;
	  }
     }
     else {
	  entry = _NewEntry(name, expr, mc);
	  assert(entry);
	  codeTable[hash] = entry;
     }
}

/*********************************
 *                               *
 *    _findEntry                 *
 *                               *
 *********************************/
static
CT_ENTRY
*_findEntry(char *name, CT_ENTRY *entry) 
{
     CT_ENTRY *curr  = NULL;

     if (entry) { 
	  curr = entry;

	  while (curr) {
	       if (strcmp(curr->name, name) == 0) {
		    return(curr);
	       }
	       curr = curr->next;
	  }
     }

     return(NULL);
     /*printMsg(ERROR_MSG, "Function %s not found in code table\n", name);  */
}

/*********************************
 *                               *
 *    CodeTableGetComb           *
 *                               *
 *********************************/
COMB_EXPR
*CodeTableGetComb(char *name)
{
     CT_ENTRY *entry = NULL;

     entry = _findEntry(name, codeTable[_hashKey(name)]);

     return(entry->comb_exp);
}

/*********************************
 *                               *
 *    CodeTableGetMC             *
 *                               *
 *********************************/
M_INSTR
*CodeTableGetMC(char *name)
{
     CT_ENTRY *entry = NULL;

     entry = _findEntry(name, codeTable[_hashKey(name)]);

     return(entry->machine_code);
}

/*********************************
 *                               *
 *    CodeTableShowComb          *
 *                               *
 *********************************/
void
CodeTableShowComb(char *funName)
{
     CT_ENTRY *entry = NULL;

     entry = _findEntry(funName, codeTable[_hashKey(funName)]);
     
     if (entry) {
       CombExprPrint(entry->comb_exp);
       printf("\n"); fflush(stdout);
     }
}

/*********************************
 *                               *
 *    CombTableShowMC            *
 *                               *
 *********************************/
void
CodeTableShowMC(char *funName)
{
     printMsg(ERROR_MSG, "CodeTableShowMC(): Not implemented yet!"); /* !!!! implement */
}


