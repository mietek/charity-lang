/******************************************************************************
 *                                                                            *
 *   pmem.c                                                                   *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "ioChar.h"
#include "pmem.h"
#include "list.h"

#define DEBUG 0

/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/
static char          *MemPageListHead(MEM_LIST_PAGE *list);
static MEM_LIST_PAGE *MemPageListTail(MEM_LIST_PAGE *list);
static MEM_LIST_PAGE *MemPageListCons(char *mem, MEM_LIST_PAGE *pages);

/*****************************************************************************
 *                                                                           *
 *                           internal variables                              *
 *                                                                           *
 *****************************************************************************/

typedef struct _MEM_TABLE {
     MEM_LIST_PAGE  *pageList;             /* A list of memory pages   */
     char           *currentPage;          /* Current page of memory   */
     int             currentIdx;           /* current free memory      */
     int             currentSize;
     int             numPages;
     char           *name;
} MEM_TABLE;

static MEM_TABLE **MemTable;     /* stores the memory alloced heaps */
static int         MemTableSize;


/*********************************
 *                               *
 *    MemPageListHead            *
 *                               *
 *********************************/
static
char
*MemPageListHead(MEM_LIST_PAGE *list)
{
     return((char *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    MemPageListTail            *
 *                               *
 *********************************/
static
MEM_LIST_PAGE
*MemPageListTail(MEM_LIST_PAGE *list)
{
     return((MEM_LIST_PAGE *) ListTail((LIST *) list));
}

/***************************************
 *                                     *
 *        MemPageListCons              *
 *                                     *
 ***************************************/
static
MEM_LIST_PAGE
*MemPageListCons(char *mem, MEM_LIST_PAGE *pages)
{
     MEM_LIST_PAGE *result = (MEM_LIST_PAGE *) calloc(1, sizeof(MEM_LIST_PAGE));
     
     assert(result);
     if (pages) {
	  assert(pages->lt == L_MEM_PAGE);
     }

     result->item = mem;
     result->next = pages;
     result->lt   = L_MEM_PAGE;

     return(result);
}

/***************************************
 *                                     *
 *        getNumPages                  *
 *                                     *
 ***************************************/
int
getNumPages(MEMORY heapDesc)
{
     assert(heapDesc >= 0);
     return(MemTable[heapDesc]->numPages);
}

/***************************************
 *                                     *
 *        getPage                      *
 *                                     *
 ***************************************/
char
*getPage(MEMORY heapDesc, int page)
{
     MEM_LIST_PAGE *tmp   = NULL;
     MEM_LIST_PAGE *curr  = NULL;
     int            count = 0;

     assert(heapDesc >= 0);
     assert(page > 0);
     
     curr = MemTable[heapDesc]->pageList;
     tmp = MemPageListTail(curr);
     
     for (count = 1; count < page; count++) {
	  curr = tmp;
	  tmp = MemPageListTail(curr);
     }
     return(MemPageListHead(curr));
}

/***************************************
 *                                     *
 *        MemConstruct                 *
 *                                     *
 ***************************************/
void
MemConstruct(int maxTableSize)
{
     MemTable = (MEM_TABLE **) calloc(maxTableSize+1, sizeof(MEM_TABLE *));
     assert(MemTable != NULL);

     MemTableSize = maxTableSize + 1;
}

/***************************************
 *                                     *
 *        MemDestruct                  *
 *                                     *
 ***************************************/
void  
MemDestruct(void)
{
     int count = 0;

     for (count = 0; count < MemTableSize; count++) {
	  if (MemTable[count]) {
	       MemDealloc((MEMORY) count);
	  }
     }
}


/***************************************
 *                                     *
 *        MemDealloc                   *
 *                                     *
 ***************************************/
void
MemDealloc(MEMORY heapDesc)
{
     assert(MemTable[heapDesc]);

#if DEBUG
     printMsg(MSG, "MemDealloc %-25s: %d/%d bytes used\n", MemTable[heapDesc]->name,
	      ((MemTable[heapDesc]->numPages-1) * MemTable[heapDesc]->currentSize) + MemTable[heapDesc]->currentIdx,
	      MemTable[heapDesc]->numPages * MemTable[heapDesc]->currentSize);
#endif

     MemReset(heapDesc);

     free(MemTable[heapDesc]->currentPage);
     /** This next line causes a seg fault on Linux **
      ** Electric Fence also complains.             **
      ** Probably memory already freed in MemReset  **/
     /* free(MemPageListHead(MemTable[heapDesc]->pageList)); */
     free(MemTable[heapDesc]->pageList);

     MemTable[heapDesc] = NULL;
}


/*********************************
 *                               *
 *    MemDisplayState            *
 *                               *
 *********************************/
void
MemDisplayState(void)
{
     int count      = 0;
     int currBytes  = 0;
     int totalBytes = 0;

     printMsg(MSG, "Summary of Memory Usage:\n");
     printMsg(MSG, "------------------------\n");
     for (count = 0; count < MemTableSize; count++) {
	  if (MemTable[count]) {
	       currBytes = ((MemTable[count]->numPages-1) * MemTable[count]->currentSize) 
  		           + MemTable[count]->currentIdx;
	       
	       printMsg(MSG, "%s: %d/%d bytes used\n", 
		      MemTable[count]->name,
		      currBytes,
		      MemTable[count]->numPages * MemTable[count]->currentSize);
	       
	       totalBytes += currBytes;
	  }
     }
     printMsg(MSG, "\nTotal Memory used:  %d bytes.\n", totalBytes);
}

/***************************************
 *                                     *
 *        MemAlloc                     *
 *                                     *
 ***************************************/
MEMORY
MemAlloc(char *name, size_t len, size_t size)
{
/* MemAlloc()
 * Allocate a heap;
 * Returns:  A memory descriptor (similar to a file descriptor)
 */

     int count = 0;
     assert(MemTable);

     /* search for a free memory heap location */
     while ((count < MemTableSize) && (MemTable[count] != NULL) ) { 
	     count++;
     }

     if (count >= MemTableSize) {
	  printMsg(FATAL_MSG, "pmem(%s): Max number of heap stores (%d) exhausted", 
		   name, MemTableSize);
     }

     MemTable[count] = (MEM_TABLE *) calloc(1, sizeof(MEM_TABLE));

     if (!MemTable[count]) {
	  printMsg(FATAL_MSG, "pmem(%s): Internal memory exhausted", name);
     }

     MemTable[count]->currentPage = (char *) calloc(len, size);

     if (!MemTable[count]->currentPage) {
	  printMsg(FATAL_MSG, "pmem(%s): Internal memory exhausted", name);
     }

     MemTable[count]->currentIdx  = 0;
     MemTable[count]->currentSize = size * len;
     MemTable[count]->name        = name;

     MemTable[count]->pageList    = MemPageListCons(MemTable[count]->currentPage, NULL);
     MemTable[count]->numPages    = 1;

#if DEBUG
     printf("Pointers: %p   %p\n", MemTable[count]->currentPage, MemTable[count]->currentPage + MemTable[count]->currentSize - 1);
     printf("Memory allocate %-25s: %d bytes\n", name, (size*len));
#endif

     return((MEMORY) count);
}

/***************************************
 *                                     *
 *        MemHeapAlloc                 *
 *                                     *
 ***************************************/
char
*MemHeapAlloc(MEMORY heapDesc, size_t len, size_t size)
{
/* MemHeapAlloc()
 * Allocate a chunk of memory in a given heap
 */

     int   count  = 0;
     int   amount = 0;
     int   align  = 0;
     int   ptrBytes = sizeof(char *);

     char *mem    = NULL;

     assert(heapDesc >= 0);
     assert(MemTable[heapDesc]);
     assert(size > 0);
     if (len == 0)
       return NULL;

     align = (ptrBytes - ((size * len) % ptrBytes)) % ptrBytes;
     amount = (size * len) + align;

     if ((MemTable[heapDesc]->currentIdx + amount) 
	 <=
	  MemTable[heapDesc]->currentSize) {
#if DEBUG
       printf("More memory for heap: %s,   ", MemTable[heapDesc]->name);
       printf("Old index: %d,  ", MemTable[heapDesc]->currentIdx);
#endif
	  mem = MemTable[heapDesc]->currentPage + MemTable[heapDesc]->currentIdx;
	  assert(mem);
	  MemTable[heapDesc]->currentIdx += amount;

	  for (count = 0; count < amount; count++) {
	       mem[count] = '\0';
	  }
#if DEBUG
	  printf("Amount: %d, ", amount);
          printf("New index: %d\n", MemTable[heapDesc]->currentIdx);
#endif
     }
     else {
	  if (amount > MemTable[heapDesc]->currentSize) {
	       printMsg(FATAL_MSG, "pmem:(%s) amount of memory requested is greater than page size",
			MemTable[heapDesc]->name);
	  }

	  mem = (char *) calloc(MemTable[heapDesc]->currentSize, sizeof(char));

	  if (!mem) {
	       printMsg(FATAL_MSG, "pmem(%s): Internal memory exhausted",
			MemTable[heapDesc]->name);
	  }

	  MemTable[heapDesc]->currentPage = mem;
	  MemTable[heapDesc]->currentIdx  = amount;

	  MemTable[heapDesc]->pageList    = MemPageListCons(mem,
							    MemTable[heapDesc]->pageList);

	  MemTable[heapDesc]->numPages++;
#if DEBUG
	  printf("\nNew page for heap %s\n", MemTable[heapDesc]->name);
     printf("Pointers: %p   %p\n", MemTable[heapDesc]->currentPage, MemTable[heapDesc]->currentPage + MemTable[heapDesc]->currentSize - 1);
       printf("Memory for heap: %s,   ", MemTable[heapDesc]->name);
       printf("New index: %d\n", MemTable[heapDesc]->currentIdx);
#endif
     }

     return(mem);
}


/***************************************
 *                                     *
 *        MemHeapReset                 *
 *                                     *
 ***************************************/
void
MemReset(MEMORY heapDesc)
{
     MEM_LIST_PAGE *tmp   = NULL;
     MEM_LIST_PAGE *curr  = NULL;
     int            count = 0;
     int            num   = 0;

     assert(MemTable[heapDesc] != NULL);

     curr = MemTable[heapDesc]->pageList;
     tmp = MemPageListTail(curr);

     num = MemTable[heapDesc]->numPages - 1;
     for (count = 0; count < num; count++) {
       free(MemPageListHead(curr));
       free(curr);
       curr = tmp;
       tmp = MemPageListTail(curr);
     }
/*
     while (tmp != NULL) {
	  if (tmp->next == curr) {
	       printMsg(FATAL_MSG, "cyclic pmem\n");
	  }
	  free(MemPageListHead(curr));
	  free(curr);
	  curr = tmp;
	  tmp = MemPageListTail(curr);
     }
*/
     MemTable[heapDesc]->currentPage = MemPageListHead(curr);
     MemTable[heapDesc]->currentIdx  = 0;
     MemTable[heapDesc]->numPages    = 1;
     MemTable[heapDesc]->pageList    = curr;
     assert(MemPageListTail(curr) == NULL); 
}
