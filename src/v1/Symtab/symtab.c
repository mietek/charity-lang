/******************************************************************************
 *                                                                            *
 *   symtab.c                                                                 *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "symtab.h"
#include "symtabI.h"
#include "ioChar.h"

ST_SCOPE *symTab = NULL;

/* hash table functions */

static int hashName (char   *key);
static int hashKey  (ST_KEY  key);

static void initSymTableHeap (void);

char *makeMacroName (char *functionName,
                     char *macroName);


/*********************************
 *                               *
 *    hashName                   *
 *                               *
 *********************************/
/* uses straightforward division algorithm for determining hash value */
static int
hashName(char *key) {

  int keySize,
      i,
      total = 0,
      size = SYMTAB_SIZE;

  keySize = strlen(key);
  for (i=0; i < keySize; i++) {
    total = total + (int)key[i];
  }
  return(total % size);

}


/*********************************
 *                               *
 *    hashKey                    *
 *                               *
 *********************************/
/* uses straightforward division algorithm for determining hash value */
static int
hashKey(ST_KEY key) {

  return((unsigned long int)key % SYMTAB_SIZE);

}


/*********************************
 *                               *
 *    initSymTable               *
 *                               *
 *********************************/

void
initSymTable (BBOOL loadBase)
{
/*
  symTabDesc = MemAlloc("symtab", SYMTAB_SIZE, sizeof(ST_ENTRY *));
  symTab = (ST_SYM_TAB *)MemHeapAlloc(symTabDesc, SYMTAB_SIZE,
                                      sizeof(ST_ENTRY *));

  initSymTableHeap ();
*/

  assert (!symTab);

  PushScope ();

  if (loadBase)
    loadBaseTypes ();
}


/*********************************
 *                               *
 *    initSymTableHeap           *
 *                               *
 *********************************/
static void
initSymTableHeap(void) {

  /* allocate space for the entries in the table */
/*
  stHD = MemAlloc("symtabHeap", SYMTAB_HEAP_SIZE, sizeof(char));
*/
}


/*********************************
 *                               *
 *    st_DestructSymTable        *
 *                               *
 *********************************/

void
st_DestructSymTable (void)
{
/*
  MemDealloc (stHD);
  MemDealloc (symTabDesc);
*/

  PopScope ();

  assert (!symTab);
}


/*********************************
 *                               *
 *    PushScope                  *
 *                               *
 *********************************/

void
PushScope (void)
{
  ST_SCOPE *newScope = NULL;
  int       index    = 0;

  newScope = calloc (1, sizeof (ST_SCOPE));

  assert (newScope);

  newScope->scopeHD = MemAlloc ("scope", SYMTAB_HEAP_SIZE, sizeof (char));
  newScope->below   = symTab;

  for (index = 0; index < SYMTAB_SIZE; index++)
    {
      newScope->table[index]     = NULL;
      newScope->lookupTab[index] = NULL;
    }

  symTab = newScope;
}


/*********************************
 *                               *
 *    PopScope                   *
 *                               *
 *********************************/

void
PopScope (void)
{
  ST_SCOPE *oldScope = symTab;

  assert (symTab);

  symTab = oldScope->below;

  MemDealloc (oldScope->scopeHD);

  free (oldScope);
}


/*********************************
 *                               *
 *    stPopToTop                 *
 *                               *
 *********************************/
void
stPopToTop(void) {

    while ( symTab->below != NULL )
        PopScope();

}


/*********************************
 *                               *
 *    addEntryToSymTab           *
 *                               *
 *********************************/
ST_KEY
addEntryToSymTab(ST_ENTRY *entry) {

  int              index;
  ST_ENTRY        *nextEntry;
  ST_LOOKUP_ENTRY *nextLookup,
                  *lookup;

/* add to lookup table */

  lookup = (ST_LOOKUP_ENTRY *)MHA(symTab->scopeHD,1, sizeof (ST_LOOKUP_ENTRY));

  lookup->next = NULL;
  lookup->name = entry->name;
  lookup->key  = entry->key;

  index = hashName(lookup->name);
  if (symTab->lookupTab[index]) {                          /* collision */
    nextLookup = symTab->lookupTab[index];
    while (nextLookup->next) {      /* traverse to end of list */
      nextLookup = nextLookup->next;
    }   /*  elihw  */
    nextLookup->next = lookup;   /* put new entry at end */
  }   /*  fi  */
  else {                                        /* no collision */
    symTab->lookupTab[index] = lookup;
  }   /*  esle  */

  /* add to symbol table */
  entry->nextEntry = NULL;
  index = hashKey(entry->key);
  if (symTab->table[index]) {                          /* collision */
    nextEntry = symTab->table[index];
    while (nextEntry->nextEntry) {      /* traverse to end of list */
      nextEntry = nextEntry->nextEntry;
    }   /*  elihw  */
    nextEntry->nextEntry = entry;   /* put new entry at end */
  }   /*  fi  */
  else {                                        /* no collision */
    symTab->table[index] = entry;
  }   /*  esle  */

  return(entry->key);

}


/*********************************
 *                               *
 *    st_RemoveEntry             *
 *                               *
 *********************************/

void
st_RemoveEntry (ST_KEY key)
{
  ST_ENTRY *entry = st_GetEntry (key, NULL);

  assert (entry);

  if (entry->tag == ST_FUNCTION ||
      entry->tag == ST_DATATYPE ||
      entry->tag == ST_ALIAS)
    st_RemoveAnyEntry (key);
  else
    printMsg (ERROR_MSG, "Not allowed to remove %s.", entry->name);
}


/*********************************
 *                               *
 *    st_RemoveAnyEntry          *
 *                               *
 *********************************/

/*
 *  [FIX] - SHOULD RELEASE MEMORY
 *        - ONLY WORKS WHEN SYMBOL TABLE STACK HEIGHT EQUALS 1
 *
 */

void
st_RemoveAnyEntry (ST_KEY key)
{
  int index      = hashKey (key);
  int numMacros  = 0;
  int numStructs = 0;
  int i          = 0;

  ST_ENTRY *before = symTab->table[index];
  ST_ENTRY *next   = symTab->table[index]->nextEntry;
  ST_ENTRY *entry  = st_GetEntry (key, NULL);

  ST_LOOKUP_ENTRY *lbefore = NULL;
  ST_LOOKUP_ENTRY *lnext   = NULL;

  assert (entry);

  switch (entry->tag)
    {
    case ST_FUNCTION:
      numMacros = entry->info.function.numMacros;

      for (i = 0; i < numMacros; i++)
        st_RemoveAnyEntry (entry->info.function.macroKeys[i]);

      break;

    case ST_DATATYPE:
      numStructs = entry->info.datatype.numStructors;

      for (i = 0; i < numStructs; i++)
        st_RemoveAnyEntry (entry->info.datatype.structorKeys[i]);

      for (i = 0; i < NUM_OPCOMBS; i++)
        st_RemoveAnyEntry (entry->info.datatype.opCombKeys[i]);

      break;

    case ST_MACRO:
    case ST_STRUCTOR:
    case ST_OPCOMB:
    case ST_ALIAS:
    case ST_VAR:

      break;

    default:
      printMsg (FATAL_MSG, "st_RemoveAnyEntry() - Invalid tag %d", entry->tag);
    }

  if (st_KeysEqual (symTab->table[index]->key, key))
    symTab->table[index] = symTab->table[index]->nextEntry;
  else
    {
      while (!st_KeysEqual (next->key, key))
        {
          before = next;
          next   = next->nextEntry;
        }

      before->nextEntry = next->nextEntry;
    }

  index   = hashName (entry->name);

  lbefore = symTab->lookupTab[index];
  lnext   = symTab->lookupTab[index]->next;

  if (strcmp (symTab->lookupTab[index]->name, entry->name) == 0)
    symTab->lookupTab[index] = symTab->lookupTab[index]->next;
  else
    {
      while (strcmp (lnext->name, entry->name) != 0)
        {
          lbefore = lnext;
          lnext   = lnext->next;
        }

      lbefore->next = lnext->next;
    }
}


/*********************************
 *                               *
 *    getEntry                   *
 *                               *
 *********************************/

ST_ENTRY *
getEntry (char *name)
{
  ST_SCOPE        *scope  = symTab;
  ST_ENTRY        *entry  = NULL;
  ST_LOOKUP_ENTRY *lookup = NULL;
  int              index  = 0;

  assert (name);

  index = hashName (name);

  while (scope)
    {
      lookup = scope->lookupTab[index];

      while (lookup)
        {
          if (strcmp (lookup->name, name) == 0)
            {
              entry = scope->table[hashKey (lookup->key)];

              while (entry)
                if (st_KeysEqual (entry->key, lookup->key))
                  return entry;
                else
                  entry = entry->nextEntry;
            }

          lookup = lookup->next;
        }

      scope = scope->below;
    }

  return NULL;
}


/*********************************
 *                               *
 *    st_GetEntry                *
 *                               *
 *********************************/

ST_ENTRY *
st_GetEntry (ST_KEY key, int *level)
{
  ST_SCOPE *scope = symTab;
  ST_ENTRY *entry = NULL;
  int       index = hashKey (key);
  int       safeLevel = 0;

  if ( level == NULL )
      level = &safeLevel;

  *level = 0;

  while (scope)
    {
      entry = scope->table[index];

      while (entry)
        {
          if (st_KeysEqual (entry->key, key))
            return entry;
          else
            entry = entry->nextEntry;
        }

      (*level)++;
      scope = scope->below;
    }

  *level = -1;
  return NULL;
}
