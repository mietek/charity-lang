/******************************************************************************
 *                                                                            *
 *   pmem.h                                                                   *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#ifndef __PMEM_H__
#define __PMEM_H__

/***********************************
 * pmem.h
 * Generic Memory manager
 ***********************************/

#define MHA   MemHeapAlloc

typedef int MEMORY;

extern void    MemDisplayState(void);

extern void    MemConstruct(int MaxTableSize);
extern void    MemDestruct(void);
extern void    MemDealloc(MEMORY heapDesc);                         /* deallocate a heap                          */
extern MEMORY  MemAlloc(char *name, size_t len, size_t size);             /* Allocate a heap                            */

extern char   *MemHeapAlloc(MEMORY heapDesc, size_t len, size_t size);    /* Null if failed allocation (heap exhausted) */

extern void    MemReset(MEMORY heapDesc);                           /* Reset heap                                 */

extern int     getNumPages(MEMORY heapDesc);
extern char   *getPage(MEMORY heapDesc, int page);
#endif

